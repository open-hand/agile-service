package io.choerodon.agile.app.service.impl;

import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.modelmapper.ModelMapper;
import org.modelmapper.TypeToken;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.ObjectUtils;

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

import org.hzero.core.base.BaseConstants;
import org.hzero.core.util.Pair;
import org.hzero.mybatis.domian.Condition;
import org.hzero.mybatis.util.Sqls;

/**
 * @author zhaotianxin 2020-08-12 10:09
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
        List<StatusTransferSettingDTO> statusTransferSettings = statusTransferSettingMapper.listByStatusId(projectId,issueTypeId,statusIds);
        if(CollectionUtils.isEmpty(statusTransferSettings)){
           return new ArrayList<>();
        }
        Long organizationId = ConvertUtil.getOrganizationId(projectId);
        Map<Long,UserDTO> userDTOMap = new HashMap<>();
        Map<Long,RoleVO> roleMap = new HashMap<>();
        processUserAndRoleMap(statusTransferSettings, organizationId, userDTOMap, roleMap);
        return statusTransferSettingAssembler.listDTOToVO(statusTransferSettings,userDTOMap, roleMap, ConvertUtil.getOrganizationId(projectId));
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
            List<UserDTO> userDTOS = userService.listUsersByIds(userIds.toArray(new Long[0]));
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
    public Boolean verifyStatusTransferSetting(Long projectId, IssueDTO issueDTO, Long endStatusId) {
        List<StatusTransferSettingDTO> statusTransferSettingList = query(projectId, issueDTO.getIssueTypeId(), endStatusId);
        if (CollectionUtils.isEmpty(statusTransferSettingList)) {
            return false;
        }
        Pair<Boolean, Boolean> pair = isCurrentAllowedAndIsVerifySub(projectId, statusTransferSettingList, issueDTO);
        Boolean isCurrentAllowed = pair.getFirst();
        Boolean verifySubIssueCompleted = pair.getSecond();
        if (!isCurrentAllowed) {
            return true;
        }
        // 校验当前问题的子级任务是否都是已解决状态
        boolean result = false;
        if (Boolean.TRUE.equals(verifySubIssueCompleted)) {
            String typeCode = issueDTO.getTypeCode();
            if (WATERFALL_ISSUE_TYPES.contains(typeCode)) {
                //瀑布类型
                if (!ObjectUtils.isEmpty(agileWaterfallService)) {
                    result = agileWaterfallService.validateAllSubIssueUnCompleted(issueDTO, projectId);
                }
            } else {
                result = validateAgileSubIssueUnCompleted(issueDTO, projectId);
            }
        }
        return result;
    }

    private boolean validateAgileSubIssueUnCompleted(IssueDTO issueDTO,
                                                     Long projectId) {
        Boolean subIssue = ("sub_task".equals(issueDTO.getTypeCode())) || ("bug".equals(issueDTO.getTypeCode()) && !ObjectUtils.isEmpty(issueDTO.getRelateIssueId()) && !Objects.equals(0L, issueDTO.getRelateIssueId()));
        if (Boolean.FALSE.equals(subIssue)) {
            IssueCountDTO issueCountDTO = issueMapper.querySubIssueCount(projectId, issueDTO.getIssueId());
            if (!Objects.equals(0, issueCountDTO.getIssueCount()) && !Objects.equals(issueCountDTO.getSuccessIssueCount(), issueCountDTO.getIssueCount())) {
                return Boolean.TRUE;
            }
        }
        return false;
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
        List<StatusTransferSettingDTO> statusTransferSettings = statusTransferSettingMapper.listByStatusId(projectId, issueTypeId, statusIds);
        if (CollectionUtils.isEmpty(statusTransferSettings)) {
            return statusIds;
        }
        List<Long> result = new ArrayList<>();
        Map<Long, List<StatusTransferSettingDTO>> statusTransferSettingMap = statusTransferSettings.stream().collect(Collectors.groupingBy(StatusTransferSettingDTO::getStatusId));
        for (Long statusId : statusIds) {
            List<StatusTransferSettingDTO> statusTransfer = statusTransferSettingMap.get(statusId);
            if (CollectionUtils.isEmpty(statusTransfer)) {
                result.add(statusId);
            } else {
                boolean isCurrentAllowed = isCurrentAllowedAndIsVerifySub(projectId, statusTransfer, issue).getFirst();
                if (isCurrentAllowed) {
                    result.add(statusId);
                }
            }
        }
        return result;
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
        List<StatusTransferSettingDTO> statusTransferSettings = statusTransferSettingMapper.listOptions(organizationId,issueTypeId,statusIds);
        if(CollectionUtils.isEmpty(statusTransferSettings)){
            return new ArrayList<>();
        }
        Map<Long, UserDTO> userDTOMap = new HashMap<>();
        Map<Long, RoleVO> roleMap = new HashMap<>();
        processUserAndRoleMap(statusTransferSettings, organizationId, userDTOMap, roleMap);
        return statusTransferSettingAssembler.listDTOToVO(statusTransferSettings, userDTOMap, roleMap, organizationId);
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
        List<Long> statusIds = statusTransferSettingMapper.queryStatusTransferByIssueTypeAndUserType(BaseConstants.DEFAULT_TENANT_ID, projectId, issueDTO.getIssueTypeId(), StatusTransferType.OTHER);
        if (CollectionUtils.isEmpty(statusIds)) {
            return new ArrayList<>();
        }
        IssueCountDTO issueCountDTO = issueMapper.querySubIssueCount(projectId, issueId);
        if (Objects.equals(0, issueCountDTO.getIssueCount()) || Objects.equals(issueCountDTO.getSuccessIssueCount(), issueCountDTO.getIssueCount())) {
            return new ArrayList<>();
        }
        return statusMapper.selectByCondition(Condition.builder(StatusDTO.class)
                .andWhere(Sqls.custom()
                        .andEqualTo(StatusDTO.FIELD_ORGANIZATION_ID, ConvertUtil.getOrganizationId(projectId))
                        .andIn(StatusDTO.FIELD_ID, statusIds))
                .build());
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
            return result;
        }
        List<StatusVO> notAllowedStatusList = modelMapper.map(notAllowedStatus, new TypeToken<List<StatusVO>>() {}.getType());
        Set<Long> statusIdSet = result.stream().map(StatusVO::getId).collect(Collectors.toSet());
        for (StatusVO status : notAllowedStatusList) {
            if (!statusIdSet.contains(status.getId())) {
                result.add(status);
            }
        }
        return result;
    }

    /**
     *
     * @param projectId projectId
     * @param statusTransferSettingList statusTransferSettingList
     * @param issue issue
     * @return 是否允许当前用户流转， 是否校验子任务已完成
     */
    private Pair<Boolean, Boolean> isCurrentAllowedAndIsVerifySub(Long projectId,
                                                                  List<StatusTransferSettingDTO> statusTransferSettingList,
                                                                  IssueDTO issue) {
        // 如果没有任何配置, 则直接返回<允许当前用户操作, 不限制子级状态>
        if(CollectionUtils.isEmpty(statusTransferSettingList)) {
            return new Pair<>(true, false);
        }
        // 如果只有一条配置, 且配置为子级状态限制, 说明不需要考虑用户校验
        // 直接返回<允许当前用户操作, 配置的子级状态限制>
        final StatusTransferSettingDTO firstStatusTransferSetting = statusTransferSettingList.get(0);
        if(statusTransferSettingList.size() == 1 && StatusTransferType.OTHER.equals(firstStatusTransferSetting.getUserType())) {
            return new Pair<>(true, firstStatusTransferSetting.getVerifySubissueCompleted());
        }
        Long issueId = null;
        Long assigneeId = null;
        Long reporterId = null;
        Long mainResponsibleId = null;
        boolean isIssueNull = ObjectUtils.isEmpty(issue);
        if (!isIssueNull) {
            issueId = issue.getIssueId();
            assigneeId = issue.getAssigneeId();
            reporterId = issue.getReporterId();
            mainResponsibleId = issue.getMainResponsibleId();
        }
        Set<Long> roleIds = new HashSet<>();
        boolean verifySubIssueCompleted = false;
        Set<Long> userIds = new HashSet<>();
        boolean dependOnIssueDetailsTypes = false;
        for (StatusTransferSettingDTO statusTransferSetting : statusTransferSettingList) {
            String userType = statusTransferSetting.getUserType();
            Long userId = statusTransferSetting.getUserId();
            //自定义字段或依赖于issue详情的字段
            boolean dependOnIssueDetails = !StatusTransferType.NOT_DEPEND_ON_ISSUE_DETAILS_TYPES.contains(userType);
            dependOnIssueDetailsTypes = dependOnIssueDetailsTypes || dependOnIssueDetails;
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
        boolean isCurrentAllowed = true;
        if (!isIssueNull || !dependOnIssueDetailsTypes) {
            Long currentUserId = DetailsHelper.getUserDetails().getUserId();
            isCurrentAllowed = userIds.contains(currentUserId);
        }
//        else {
//            处理拖动流转限制的情况
//            issue为空，同时还要根据issue详情判断，为了防止限定范围变小，需要这里跳过校验，由/not_allowed_transfer接口判断
//            isCurrentAllowed = true;
//        }
        return Pair.of(isCurrentAllowed, verifySubIssueCompleted);
    }

    private void baseInsert(StatusTransferSettingDTO statusTransferSettingDTO) {
        if (statusTransferSettingMapper.insertSelective(statusTransferSettingDTO) != 1) {
            throw new CommonException("error.insert.status.transfer.setting");
        }
    }
}
