package io.choerodon.agile.app.service.impl;

import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.app.assembler.StatusTransferSettingAssembler;
import io.choerodon.agile.app.service.*;
import io.choerodon.agile.infra.dto.IssueCountDTO;
import io.choerodon.agile.infra.dto.StatusDTO;
import io.choerodon.agile.infra.dto.StatusTransferSettingDTO;
import io.choerodon.agile.infra.dto.UserDTO;
import io.choerodon.agile.infra.dto.business.IssueDTO;
import io.choerodon.agile.infra.enums.IssueTypeCode;
import io.choerodon.agile.infra.enums.StatusTransferType;
import io.choerodon.agile.infra.feign.operator.RemoteIamOperator;
import io.choerodon.agile.infra.mapper.*;
import io.choerodon.agile.infra.utils.AssertUtilsForCommonException;
import io.choerodon.agile.infra.utils.ConvertUtil;
import io.choerodon.core.exception.CommonException;
import io.choerodon.core.oauth.DetailsHelper;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.hzero.core.util.Pair;
import org.hzero.mybatis.domian.Condition;
import org.hzero.mybatis.util.Sqls;
import org.modelmapper.ModelMapper;
import org.modelmapper.TypeToken;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.ObjectUtils;

/**
 * @author zhaotianxin
 * @date 2020-08-12 10:09
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class StatusTransferSettingServiceImpl implements StatusTransferSettingService {
    @Autowired
    private StatusTransferSettingMapper statusTransferSettingMapper;
    @Autowired
    private UserService userService;
    @Autowired
    private StatusTransferSettingAssembler statusTransferSettingAssembler;
    @Autowired
    private StatusMapper statusMapper;
    @Autowired
    private RemoteIamOperator remoteIamOperator;
    @Autowired
    private ProjectConfigService projectConfigService;
    @Autowired
    private OrganizationConfigService organizationConfigService;
    @Autowired
    private IssueMapper issueMapper;
    @Autowired
    private ModelMapper modelMapper;
    @Autowired(required = false)
    private AgileWaterfallService agileWaterfallService;
    @Autowired
    private IssueParticipantRelMapper issueParticipantRelMapper;
    @Autowired
    private IssueUserRelMapper issueUserRelMapper;
    @Autowired
    private FieldValueMapper fieldValueMapper;

    private static final List<String> WATERFALL_ISSUE_TYPES = Arrays.asList(IssueTypeCode.WATERFALL_ISSUE_TYPE_CODE);

    @Override
    public void createOrUpdate(Long projectId, Long issueTypeId, Long statusId,Long objectVersionNumber,String applyType,List<StatusTransferSettingCreateVO> list) {
        List<StatusTransferSettingDTO> query = query(projectId, issueTypeId, statusId);
        if (!CollectionUtils.isEmpty(query)) {
            delete(projectId, issueTypeId, statusId);
        }
        if (!CollectionUtils.isEmpty(list)) {
            for (StatusTransferSettingCreateVO settingCreateVO : list) {
                String type = settingCreateVO.getType();
                boolean containsUserIds = (StatusTransferType.isSpecifier(type) || StatusTransferType.isRole(type));
                if (containsUserIds
                        && !CollectionUtils.isEmpty(settingCreateVO.getUserIds())) {
                    for (Long userId : settingCreateVO.getUserIds()) {
                        StatusTransferSettingDTO statusTransferSettingDTO = new StatusTransferSettingDTO(issueTypeId, statusId, projectId, type);
                        statusTransferSettingDTO.setUserId(userId);
                        baseInsert(statusTransferSettingDTO);
                    }
                } else {
                    StatusTransferSettingDTO statusTransferSettingDTO = modelMapper.map(settingCreateVO, StatusTransferSettingDTO.class);
                    statusTransferSettingDTO.setIssueTypeId(issueTypeId);
                    statusTransferSettingDTO.setStatusId(statusId);
                    statusTransferSettingDTO.setProjectId(projectId);
                    statusTransferSettingDTO.setUserType(type);
                    baseInsert(statusTransferSettingDTO);
                }
            }
            projectConfigService.updateNodeObjectVersionNumber(projectId,issueTypeId,statusId,objectVersionNumber,applyType);
        }
    }

    @Override
    public List<StatusTransferSettingDTO> query(Long projectId, Long issueTypeId, Long statusId) {
        StatusTransferSettingDTO statusTransferSettingDTO = new StatusTransferSettingDTO();
        statusTransferSettingDTO.setStatusId(statusId);
        statusTransferSettingDTO.setProjectId(projectId);
        statusTransferSettingDTO.setIssueTypeId(issueTypeId);
        return statusTransferSettingMapper.select(statusTransferSettingDTO);
    }

    @Override
    public void delete(Long projectId, Long issueTypeId, Long statusId) {
        StatusTransferSettingDTO statusTransferSettingDTO = new StatusTransferSettingDTO();
        statusTransferSettingDTO.setStatusId(statusId);
        statusTransferSettingDTO.setProjectId(projectId);
        statusTransferSettingDTO.setIssueTypeId(issueTypeId);
        statusTransferSettingMapper.delete(statusTransferSettingDTO);
    }


    public void deleteByOrgId(Long organizationId, Long issueTypeId, Long statusId) {
        StatusTransferSettingDTO statusTransferSettingDTO = new StatusTransferSettingDTO();
        statusTransferSettingDTO.setStatusId(statusId);
        statusTransferSettingDTO.setProjectId(0L);
        statusTransferSettingDTO.setOrganizationId(organizationId);
        statusTransferSettingDTO.setIssueTypeId(issueTypeId);
        statusTransferSettingMapper.delete(statusTransferSettingDTO);
    }

    @Override
    public List<StatusTransferSettingVO> listByStatusIds(Long projectId, Long issueTypeId, List<Long> statusIds) {
        if (CollectionUtils.isEmpty(statusIds)) {
            throw new CommonException("error.statusIds.null");
        }
        List<StatusTransferSettingDTO> dtos = statusTransferSettingMapper.listByStatusId(projectId,issueTypeId,statusIds);
        if(CollectionUtils.isEmpty(dtos)){
           return new ArrayList<>();
        }
        Long organizationId = ConvertUtil.getOrganizationId(projectId);
        Map<Long,UserDTO> userDTOMap = new HashMap<>();
        Map<Long,RoleVO> roleMap = new HashMap<>();
        processUserAndRoleMap(dtos, organizationId, userDTOMap, roleMap);
        return statusTransferSettingAssembler.listDTOToVO(dtos,userDTOMap, roleMap, ConvertUtil.getOrganizationId(projectId));
    }

    private void processUserAndRoleMap(List<StatusTransferSettingDTO> statusTransferSettingList,
                                       Long organizationId,
                                       Map<Long, UserDTO> userMap,
                                       Map<Long, RoleVO> roleMap) {
        Set<Long> userIds = new HashSet<>();
        Set<Long> roleIds = new HashSet<>();
        statusTransferSettingList.forEach(x -> {
            String userType = x.getUserType();
            Long userId = x.getUserId();
            if (StatusTransferType.isSpecifier(userType)) {
                userIds.add(userId);
            } else if (StatusTransferType.isRole(userType)) {
                roleIds.add(userId);
            }
        });
        if(!CollectionUtils.isEmpty(userIds)){
            List<UserDTO> userDTOS = userService.listUsersByIds(userIds.toArray(new Long[userIds.size()]));
            userMap.putAll(userDTOS.stream().collect(Collectors.toMap(UserDTO::getId, Function.identity())));
        }
        if (!ObjectUtils.isEmpty(roleIds)) {

            List<RoleVO> roles =
                    remoteIamOperator.listRolesByIds(organizationId, new ArrayList<>(roleIds));
            roleMap.putAll(roles.stream().collect(Collectors.toMap(RoleVO::getId, Function.identity())));
        }
    }

    @Override
    public void checkStatusTransferSetting(Long projectId, IssueDTO issueDTO, Long endStatusId) {
        Boolean verifyStatusTransferSetting = verifyStatusTransferSetting(projectId, issueDTO, endStatusId);
        if (Boolean.TRUE.equals(verifyStatusTransferSetting)) {
            StatusDTO statusDTO = statusMapper.selectByPrimaryKey(endStatusId);
            throw new CommonException("error.no.permission.to.switch", statusDTO.getName());
        }
    }

    @Override
    public Boolean verifyStatusTransferSetting(Long projectId, IssueDTO issueDTO, Long endStatusId){
        List<StatusTransferSettingDTO> statusTransferSettingList = query(projectId, issueDTO.getIssueTypeId(), endStatusId);
        if (CollectionUtils.isEmpty(statusTransferSettingList)) {
            return Boolean.FALSE;
        }
        // 获取当前的用户
        Long userId = DetailsHelper.getUserDetails().getUserId();
        Pair<Set<Long>, Boolean> pair = queryUserIdsAndVerify(projectId, statusTransferSettingList, issueDTO);
        Set<Long> userIds = pair.getFirst();
        Boolean verifySubIssueCompleted = pair.getSecond();
        if (!CollectionUtils.isEmpty(userIds) && !userIds.contains(userId)) {
            return Boolean.TRUE;
        }
        // 校验当前问题的子级任务是否都是已解决状态
        if (Boolean.TRUE.equals(verifySubIssueCompleted)) {
            Boolean subIssue = ("sub_task".equals(issueDTO.getTypeCode())) || ("bug".equals(issueDTO.getTypeCode()) && !ObjectUtils.isEmpty(issueDTO.getRelateIssueId()) && !Objects.equals(0L, issueDTO.getRelateIssueId()));
            if (Boolean.FALSE.equals(subIssue)) {
                IssueCountDTO issueCountDTO = issueMapper.querySubIssueCount(projectId, issueDTO.getIssueId());
                if (!Objects.equals(0, issueCountDTO.getIssueCount()) && !Objects.equals(issueCountDTO.getSuccessIssueCount(), issueCountDTO.getIssueCount())) {
                    return Boolean.TRUE;
                }
            }
        }
        return Boolean.FALSE;
    }

    private void queryUserIdsByRoleIds(Long projectId, Set<Long> userIds, Set<Long> roleIds) {
        if (!roleIds.isEmpty()) {
            //查角色下的用户
            String idString = StringUtils.join(roleIds, ",");
            List<UserVO> users = remoteIamOperator.listUsersUnderRoleByIds(projectId, idString);
            if (!ObjectUtils.isEmpty(users)) {
                userIds.addAll(users.stream().map(UserVO::getId).collect(Collectors.toList()));
            }
        }
    }

    @Override
    public List<Long> checkStatusTransform(Long projectId,
                                           List<Long> statusIds,
                                           Long issueId,
                                           Long issueTypeId) {
        IssueDTO issue = null;
        if (!ObjectUtils.isEmpty(issueId)) {
            issue = issueMapper.selectByPrimaryKey(issueId);
        }
        List<StatusTransferSettingDTO> dtos = statusTransferSettingMapper.listByStatusId(projectId, issueTypeId, statusIds);
        if (CollectionUtils.isEmpty(dtos)) {
            return statusIds;
        }
        List<Long> list = new ArrayList<>();
        Map<Long, List<StatusTransferSettingDTO>> statusTransferSettingMap = dtos.stream().collect(Collectors.groupingBy(StatusTransferSettingDTO::getStatusId));
        for (Long statusId : statusIds) {
            List<StatusTransferSettingDTO> statusTransfer = statusTransferSettingMap.get(statusId);
            if (CollectionUtils.isEmpty(statusTransfer)) {
                list.add(statusId);
            } else {
                Set<Long> userIds = queryUserIdsAndVerify(projectId, statusTransfer, issue).getFirst();
                Long userId = DetailsHelper.getUserDetails().getUserId();
                if (CollectionUtils.isEmpty(userIds) || userIds.contains(userId)) {
                    list.add(statusId);
                }
            }
        }
        return list;
    }

    @Override
    public void saveStatusTransfer(Long organizationId, Long issueTypeId, Long statusId, Long objectVersionNumber, List<StatusTransferSettingCreateVO> list) {
        List<StatusTransferSettingDTO> query = listByOptions(organizationId, issueTypeId, statusId);
        if (!CollectionUtils.isEmpty(query)) {
            deleteByOrgId(organizationId, issueTypeId, statusId);
        }
        if (!CollectionUtils.isEmpty(list)) {
            for (StatusTransferSettingCreateVO settingCreateVO : list) {
                String type = settingCreateVO.getType();
                boolean containsUserIds = (StatusTransferType.isSpecifier(type) || StatusTransferType.isRole(type));
                if (containsUserIds && !CollectionUtils.isEmpty(settingCreateVO.getUserIds())) {
                    for (Long userId : settingCreateVO.getUserIds()) {
                        StatusTransferSettingDTO statusTransferSettingDTO = new StatusTransferSettingDTO(issueTypeId, statusId, 0L, type);
                        statusTransferSettingDTO.setOrganizationId(organizationId);
                        statusTransferSettingDTO.setUserId(userId);
                        baseInsert(statusTransferSettingDTO);
                    }
                } else {
                    StatusTransferSettingDTO statusTransferSettingDTO = modelMapper.map(settingCreateVO, StatusTransferSettingDTO.class);
                    statusTransferSettingDTO.setIssueTypeId(issueTypeId);
                    statusTransferSettingDTO.setStatusId(statusId);
                    statusTransferSettingDTO.setProjectId(0L);
                    statusTransferSettingDTO.setUserType(type);
                    statusTransferSettingDTO.setOrganizationId(organizationId);
                    baseInsert(statusTransferSettingDTO);
                }
            }
            organizationConfigService.updateNodeObjectVersionNumber(organizationId,issueTypeId,statusId,objectVersionNumber);
        }
    }

    @Override
    public List<StatusTransferSettingDTO> listByOptions(Long organizationId, Long issueTypeId, Long statusId) {
        StatusTransferSettingDTO statusTransferSettingDTO = new StatusTransferSettingDTO();
        statusTransferSettingDTO.setStatusId(statusId);
        statusTransferSettingDTO.setProjectId(0L);
        statusTransferSettingDTO.setOrganizationId(organizationId);
        statusTransferSettingDTO.setIssueTypeId(issueTypeId);
        return statusTransferSettingMapper.select(statusTransferSettingDTO);
    }

    @Override
    public List<StatusTransferSettingVO> listStatusTransfer(Long organizationId, Long issueTypeId, List<Long> statusIds) {
        if (CollectionUtils.isEmpty(statusIds)) {
            throw new CommonException("error.statusIds.null");
        }
        List<StatusTransferSettingDTO> dtos = statusTransferSettingMapper.listOptions(organizationId,issueTypeId,statusIds);
        if(CollectionUtils.isEmpty(dtos)){
            return new ArrayList<>();
        }
        Map<Long, UserDTO> userDTOMap = new HashMap<>();
        Map<Long, RoleVO> roleMap = new HashMap<>();
        processUserAndRoleMap(dtos, organizationId, userDTOMap, roleMap);
        return statusTransferSettingAssembler.listDTOToVO(dtos, userDTOMap, roleMap, organizationId);
    }

    @Override
    public List<StatusVO> queryNotAllowedTransferStatus(Long projectId, Long issueId) {
        IssueDTO issueDTO = issueMapper.selectByPrimaryKey(issueId);
        AssertUtilsForCommonException.notNull(issueDTO, "error.issue.not.existed");
        String typeCode = issueDTO.getTypeCode();
        List<StatusDTO> notAllowedStatus = new ArrayList<>();
        if (WATERFALL_ISSUE_TYPES.contains(typeCode)) {
            if (!ObjectUtils.isEmpty(agileWaterfallService)) {
                notAllowedStatus.addAll(agileWaterfallService.queryWaterfallNotAllowedTransferStatus(issueDTO));
            }
        } else {
            notAllowedStatus.addAll(queryAgileNotAllowedTransferStatus(issueDTO));
        }
        return filterByStatusTransform(issueDTO, notAllowedStatus);
    }

    private List<StatusDTO> queryAgileNotAllowedTransferStatus(IssueDTO issueDTO) {
        Long projectId = issueDTO.getProjectId();
        Long issueId = issueDTO.getIssueId();
        Boolean subIssue = ("sub_task".equals(issueDTO.getTypeCode())) || ("bug".equals(issueDTO.getTypeCode()) && !ObjectUtils.isEmpty(issueDTO.getRelateIssueId()) && !Objects.equals(0L, issueDTO.getRelateIssueId()));
        if (Boolean.TRUE.equals(subIssue)) {
            return new ArrayList<>();
        }
        //校验任务子级需全部到达已解决配置
        List<Long> statusIds = statusTransferSettingMapper.queryStatusTransferByIssueTypeAndUserType(0L, projectId, issueDTO.getIssueTypeId(), "other");
        if (CollectionUtils.isEmpty(statusIds)) {
            return new ArrayList<>();
        }
        IssueCountDTO issueCountDTO = issueMapper.querySubIssueCount(projectId, issueId);
        if (Objects.equals(0, issueCountDTO.getIssueCount()) || Objects.equals(issueCountDTO.getSuccessIssueCount(), issueCountDTO.getIssueCount())) {
            return new ArrayList<>();
        }
        List<StatusDTO> statusList = statusMapper.selectByCondition(Condition.builder(StatusDTO.class)
                .andWhere(Sqls.custom().andEqualTo("organizationId", ConvertUtil.getOrganizationId(projectId))
                        .andIn("id", statusIds)).build());
        return statusList;
    }

    private List<StatusVO> filterByStatusTransform(IssueDTO issueDTO,
                                                   List<StatusDTO> notAllowedStatus) {
        Long projectId = issueDTO.getProjectId();
        String typeCode = issueDTO.getTypeCode();
        Long issueTypeId = issueDTO.getIssueTypeId();
        String applyType = projectConfigService.getApplyTypeByTypeCode(projectId, typeCode);
        List<StatusVO> statusList = projectConfigService.queryStatusByIssueTypeId(projectId, issueTypeId, applyType);
        List<Long> statusIds = statusList.stream().map(StatusVO::getId).collect(Collectors.toList());
        List<Long> canTransformStatusIds = checkStatusTransform(projectId, statusIds, issueDTO.getIssueId(), issueTypeId);
        List<StatusVO> result = new ArrayList<>();
        for (StatusVO status : statusList) {
            Long id = status.getId();
            if (!canTransformStatusIds.contains(id)) {
                result.add(status);
            }
        }
        if (ObjectUtils.isEmpty(notAllowedStatus)) {
            List<StatusVO> notAllowedStatusList = modelMapper.map(notAllowedStatus, new TypeToken<List<StatusVO>>() {
            }.getType());
            Set<Long> statusIdSet = result.stream().map(StatusVO::getId).collect(Collectors.toSet());
            for (StatusVO status : notAllowedStatusList) {
                if (!statusIdSet.contains(status.getId())) {
                    result.add(status);
                }
            }
        }
        return result;
    }

    /**
     *
     * @param projectId
     * @param StatusTransferSettingList
     * @param issue
     * @return 用户集合， 是否校验子任务已完成
     */
    private Pair<Set<Long>, Boolean> queryUserIdsAndVerify(Long projectId,
                                                           List<StatusTransferSettingDTO> StatusTransferSettingList,
                                                           IssueDTO issue) {
        Long issueId = null;
        Long assigneeId = null;
        Long reporterId = null;
        Long mainResponsibleId = null;
        if (!ObjectUtils.isEmpty(issue)) {
            issueId = issue.getIssueId();
            assigneeId = issue.getAssigneeId();
            reporterId = issue.getReporterId();
            mainResponsibleId = issue.getMainResponsibleId();
        }
        Set<Long> roleIds = new HashSet<>();
        boolean verifySubIssueCompleted = false;
        Set<Long> userIds = new HashSet<>();
        for (StatusTransferSettingDTO statusTransferSetting : StatusTransferSettingList) {
            String userType = statusTransferSetting.getUserType();
            Long userId = statusTransferSetting.getUserId();
            switch (userType) {
                case StatusTransferType.SPECIFIER:
                    userIds.add(userId);
                    break;
                case StatusTransferType.ROLE:
                    roleIds.add(userId);
                    break;
                case StatusTransferType.OTHER:
                    verifySubIssueCompleted = statusTransferSetting.getVerifySubissueCompleted();
                    break;
                case StatusTransferType.ASSIGNEE:
                    if (!ObjectUtils.isEmpty(assigneeId)) {
                        userIds.add(assigneeId);
                    }
                    break;
                case StatusTransferType.REPORTER:
                    if (!ObjectUtils.isEmpty(reporterId)) {
                        userIds.add(reporterId);
                    }
                    break;
                case StatusTransferType.PARTICIPANT:
                    if (!ObjectUtils.isEmpty(issueId)) {
                        userIds.addAll(issueParticipantRelMapper.listByIssueId(projectId, issueId));
                    }
                    break;
                case StatusTransferType.MAIN_RESPONSIBLE:
                    if (!ObjectUtils.isEmpty(mainResponsibleId)) {
                        userIds.add(mainResponsibleId);
                    }
                    break;
                case StatusTransferType.RELATED_PARTIES:
                    if (!ObjectUtils.isEmpty(issueId)) {
                        userIds.addAll(issueUserRelMapper.listUserIdsByIssueId(projectId, issueId, "relatedParties"));
                    }
                    break;
                default:
                    // 不在默认配置里，则检索自定义字段，有则加入，没有则忽略
                    if (!ObjectUtils.isEmpty(issueId)) {
                        userIds.addAll(fieldValueMapper.selectUserIdByField(projectId, Collections.singletonList(userType), issueId));
                    }
                    break;
            }
        }
        queryUserIdsByRoleIds(projectId, userIds, roleIds);
        return Pair.of(userIds, verifySubIssueCompleted);
    }

    private void baseInsert(StatusTransferSettingDTO statusTransferSettingDTO) {
        if (statusTransferSettingMapper.insertSelective(statusTransferSettingDTO) != 1) {
            throw new CommonException("error.insert.status.transfer.setting");
        }
    }
}
