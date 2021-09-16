package io.choerodon.agile.app.service.impl;

import io.choerodon.agile.api.vo.StatusTransferSettingCreateVO;
import io.choerodon.agile.api.vo.StatusTransferSettingVO;
import io.choerodon.agile.api.vo.UserVO;
import io.choerodon.agile.app.assembler.StatusTransferSettingAssembler;
import io.choerodon.agile.app.service.OrganizationConfigService;
import io.choerodon.agile.app.service.ProjectConfigService;
import io.choerodon.agile.app.service.StatusTransferSettingService;
import io.choerodon.agile.app.service.UserService;
import io.choerodon.agile.infra.dto.*;
import io.choerodon.agile.infra.dto.business.IssueDTO;
import io.choerodon.agile.infra.feign.BaseFeignClient;
import io.choerodon.agile.infra.mapper.IssueMapper;
import io.choerodon.agile.infra.mapper.StatusMapper;
import io.choerodon.agile.infra.mapper.StatusTransferSettingMapper;
import io.choerodon.agile.infra.utils.ConvertUtil;
import io.choerodon.core.exception.CommonException;
import io.choerodon.core.oauth.DetailsHelper;
import org.apache.commons.collections.CollectionUtils;
import org.hzero.mybatis.domian.Condition;
import org.hzero.mybatis.util.Sqls;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.ObjectUtils;

import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * @author zhaotianxin
 * @date 2020-08-12 10:09
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class StatusTransferSettingServiceImpl implements StatusTransferSettingService {
    private static final String SPECIFIER = "specifier";
    private static final String PROJECT_OWNER = "projectOwner";
    private static final String  OTHER = "other";
    @Autowired
    private StatusTransferSettingMapper statusTransferSettingMapper;
    @Autowired
    private UserService userService;
    @Autowired
    private StatusTransferSettingAssembler statusTransferSettingAssembler;
    @Autowired
    private StatusMapper statusMapper;
    @Autowired
    private BaseFeignClient baseFeignClient;
    @Autowired
    private ProjectConfigService projectConfigService;
    @Autowired
    private OrganizationConfigService organizationConfigService;
    @Autowired
    private IssueMapper issueMapper;
    @Autowired
    private ModelMapper modelMapper;

    @Override
    public void createOrUpdate(Long projectId, Long issueTypeId, Long statusId,Long objectVersionNumber,String applyType,List<StatusTransferSettingCreateVO> list) {
        List<StatusTransferSettingDTO> query = query(projectId, issueTypeId, statusId);
        if (!CollectionUtils.isEmpty(query)) {
            delete(projectId, issueTypeId, statusId);
        }
        if (!CollectionUtils.isEmpty(list)) {
            for (StatusTransferSettingCreateVO settingCreateVO : list) {
                if (SPECIFIER.equals(settingCreateVO.getType()) && !CollectionUtils.isEmpty(settingCreateVO.getUserIds())) {
                    for (Long userId : settingCreateVO.getUserIds()) {
                        StatusTransferSettingDTO statusTransferSettingDTO = new StatusTransferSettingDTO(issueTypeId, statusId, projectId, settingCreateVO.getType());
                        statusTransferSettingDTO.setUserId(userId);
                        baseInsert(statusTransferSettingDTO);
                    }
                } else {
                    StatusTransferSettingDTO statusTransferSettingDTO = modelMapper.map(settingCreateVO, StatusTransferSettingDTO.class);
                    statusTransferSettingDTO.setIssueTypeId(issueTypeId);
                    statusTransferSettingDTO.setStatusId(statusId);
                    statusTransferSettingDTO.setProjectId(projectId);
                    statusTransferSettingDTO.setUserType(settingCreateVO.getType());
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
        Set<Long> userIds = dtos.stream().filter(v -> !ObjectUtils.isEmpty(v.getUserId())).map(StatusTransferSettingDTO::getUserId).collect(Collectors.toSet());
        Map<Long,UserDTO> userDTOMap = new HashMap<>();
        if(!CollectionUtils.isEmpty(userIds)){
            List<UserDTO> userDTOS = userService.listUsersByIds(userIds.toArray(new Long[userIds.size()]));
            userDTOMap.putAll(userDTOS.stream().collect(Collectors.toMap(UserDTO::getId, Function.identity())));
        }
        return statusTransferSettingAssembler.listDTOToVO(dtos,userDTOMap);
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
        List<StatusTransferSettingDTO> query = query(projectId, issueDTO.getIssueTypeId(), endStatusId);
        if (CollectionUtils.isEmpty(query)) {
            return Boolean.FALSE;
        }
        // 获取当前的用户
        Long userId = DetailsHelper.getUserDetails().getUserId();
        Set<Long> userIds = new HashSet<>();
        Boolean verifySubIssueCompleted = false;
        for (StatusTransferSettingDTO statusTransferSettingDTO : query) {
            if (PROJECT_OWNER.equals(statusTransferSettingDTO.getUserType())) {
                List<UserVO> body = baseFeignClient.listProjectOwnerById(projectId).getBody();
                if (!CollectionUtils.isEmpty(body)) {
                    userIds.addAll(body.stream().map(UserVO::getId).collect(Collectors.toSet()));
                }
            } else if (SPECIFIER.equals(statusTransferSettingDTO.getUserType())) {
                userIds.add(statusTransferSettingDTO.getUserId());
            } else if (OTHER.equals(statusTransferSettingDTO.getUserType())) {
                verifySubIssueCompleted = statusTransferSettingDTO.getVerifySubissueCompleted();
            }
        }
        if (!CollectionUtils.isEmpty(userIds) && !userIds.contains(userId)) {
            return Boolean.TRUE;
        }
        // 校验当前问题的子级任务是否都是已解决状态
        if (Boolean.TRUE.equals(verifySubIssueCompleted)) {
            Boolean subIssue = ("sub_task".equals(issueDTO.getTypeCode())) || ("bug".equals(issueDTO.getTypeCode()) && !ObjectUtils.isEmpty(issueDTO.getRelateIssueId()) && !Objects.equals(0L, issueDTO.getRelateIssueId()));
            if (Boolean.FALSE.equals(subIssue)) {
                IssueCountDTO issueCountDTO = issueMapper.querySubIssueCount(projectId, issueDTO.getIssueId());
                if (!Objects.equals(issueCountDTO.getSuccessIssueCount(), issueCountDTO.getIssueCount())) {
                    return Boolean.TRUE;
                }
            }
        }
        return Boolean.FALSE;
    }

    @Override
    public List<Long> checkStatusTransform(Long projectId, Long issueTypeId, List<Long> statusIds) {
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
                Set<Long> userIds = new HashSet<>();
                getUserIds(projectId, userIds, statusTransfer);
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
                if (SPECIFIER.equals(settingCreateVO.getType()) && !CollectionUtils.isEmpty(settingCreateVO.getUserIds())) {
                    for (Long userId : settingCreateVO.getUserIds()) {
                        StatusTransferSettingDTO statusTransferSettingDTO = new StatusTransferSettingDTO(issueTypeId, statusId, 0L, settingCreateVO.getType());
                        statusTransferSettingDTO.setOrganizationId(organizationId);
                        statusTransferSettingDTO.setUserId(userId);
                        baseInsert(statusTransferSettingDTO);
                    }
                } else {
                    StatusTransferSettingDTO statusTransferSettingDTO = modelMapper.map(settingCreateVO, StatusTransferSettingDTO.class);
                    statusTransferSettingDTO.setIssueTypeId(issueTypeId);
                    statusTransferSettingDTO.setStatusId(statusId);
                    statusTransferSettingDTO.setProjectId(0L);
                    statusTransferSettingDTO.setUserType(settingCreateVO.getType());
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
        Set<Long> userIds = dtos.stream().filter(v -> !ObjectUtils.isEmpty(v.getUserId())).map(StatusTransferSettingDTO::getUserId).collect(Collectors.toSet());
        Map<Long,UserDTO> userDTOMap = new HashMap<>();
        if(!CollectionUtils.isEmpty(userIds)){
            List<UserDTO> userDTOS = userService.listUsersByIds(userIds.toArray(new Long[userIds.size()]));
            userDTOMap.putAll(userDTOS.stream().collect(Collectors.toMap(UserDTO::getId, Function.identity())));
        }
        return statusTransferSettingAssembler.listDTOToVO(dtos,userDTOMap);
    }

    @Override
    public List<StatusDTO> queryNotAllowedTransferStatus(Long projectId, Long issueId) {
        IssueDTO issueDTO = issueMapper.selectByPrimaryKey(issueId);
        Boolean subIssue = ("sub_task".equals(issueDTO.getTypeCode())) || ("bug".equals(issueDTO.getTypeCode()) && !ObjectUtils.isEmpty(issueDTO.getRelateIssueId()) && !Objects.equals(0L, issueDTO.getRelateIssueId()));
        if (Boolean.TRUE.equals(subIssue)) {
            return new ArrayList<>();
        }
        List<Long> statusIds = statusTransferSettingMapper.queryStatusTransferByIssueTypeAndUserType(0L, projectId, issueDTO.getIssueTypeId(), "other");
        if (CollectionUtils.isEmpty(statusIds)) {
            return new ArrayList<>();
        }
        IssueCountDTO issueCountDTO = issueMapper.querySubIssueCount(projectId, issueId);
        if (Objects.equals(issueCountDTO.getSuccessIssueCount(), issueCountDTO.getIssueCount())) {
            return new ArrayList<>();
        }
        List<StatusDTO> statusList = statusMapper.selectByCondition(Condition.builder(StatusDTO.class)
                .andWhere(Sqls.custom().andEqualTo("organizationId", ConvertUtil.getOrganizationId(projectId))
                        .andIn("id", statusIds)).build());
        return statusList;
    }

    private void getUserIds(Long projectId, Set<Long> userIds, List<StatusTransferSettingDTO> query){
        for (StatusTransferSettingDTO statusTransferSettingDTO : query) {
            if (PROJECT_OWNER.equals(statusTransferSettingDTO.getUserType())) {
                List<UserVO> body = baseFeignClient.listProjectOwnerById(projectId).getBody();
                if (!CollectionUtils.isEmpty(body)) {
                    userIds.addAll(body.stream().map(UserVO::getId).collect(Collectors.toSet()));
                }
            } else if (SPECIFIER.equals(statusTransferSettingDTO.getUserType())) {
                userIds.add(statusTransferSettingDTO.getUserId());
            }
        }
    }
    private void baseInsert(StatusTransferSettingDTO statusTransferSettingDTO) {
        if (statusTransferSettingMapper.insertSelective(statusTransferSettingDTO) != 1) {
            throw new CommonException("error.insert.status.transfer.setting");
        }
    }
}
