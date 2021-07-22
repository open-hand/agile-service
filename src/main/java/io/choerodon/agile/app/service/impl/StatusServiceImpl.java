package io.choerodon.agile.app.service.impl;

import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.app.service.*;
import io.choerodon.agile.infra.dto.*;
import io.choerodon.agile.infra.enums.*;
import io.choerodon.agile.infra.feign.BaseFeignClient;
import io.choerodon.agile.infra.utils.AssertUtilsForCommonException;
import io.choerodon.agile.infra.utils.ConvertUtil;
import io.choerodon.core.domain.Page;
import io.choerodon.agile.infra.cache.InstanceCache;
import io.choerodon.agile.infra.exception.RemoveStatusException;
import io.choerodon.agile.infra.mapper.*;
import io.choerodon.agile.infra.utils.EnumUtil;
import io.choerodon.agile.infra.utils.PageUtil;
import io.choerodon.core.domain.PageInfo;
import io.choerodon.core.oauth.DetailsHelper;
import io.choerodon.mybatis.pagehelper.PageHelper;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;
import io.choerodon.core.exception.CommonException;
import org.hzero.core.message.MessageAccessor;
import org.modelmapper.ModelMapper;
import org.modelmapper.TypeToken;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;
import org.springframework.util.StringUtils;

import java.util.*;
import java.util.stream.Collectors;

/**
 * @author peng.jiang, dinghuang123@gmail.com
 */
@Service
public class StatusServiceImpl implements StatusService {
    @Autowired
    private StatusMapper statusMapper;
    @Autowired
    private StateMachineNodeDraftMapper nodeDraftMapper;
    @Autowired
    private StatusMachineNodeMapper nodeDeployMapper;
    @Autowired
    private StateMachineNodeService nodeService;
    @Autowired
    private StateMachineTransformDraftMapper transformDraftMapper;
    @Autowired
    private StatusMachineTransformMapper transformDeployMapper;
    @Autowired
    private InstanceCache instanceCache;
    @Autowired
    private StatusMachineMapper statusMachineMapper;
    @Autowired
    private ModelMapper modelMapper;
    @Autowired
    private IssueMapper issueMapper;
    @Autowired
    private ProjectConfigService projectConfigService;
    @Autowired
    private IssueStatusService issueStatusService;
    @Autowired
    private BaseFeignClient baseFeignClient;
    @Autowired
    private IssueStatusMapper issueStatusMapper;
    @Autowired
    private IssueTypeMapper issueTypeMapper;
    @Autowired
    private IssueService issueService;
    @Autowired
    private ProjectConfigMapper projectConfigMapper;
    @Autowired
    private StateMachineSchemeConfigService stateMachineSchemeConfigService;

    @Override
    public Page<StatusWithInfoVO> queryStatusList(PageRequest pageRequest, Long organizationId, StatusSearchVO statusSearchVO) {
        Page<Long> statusIdsPage = PageHelper.doPageAndSort(pageRequest, () -> statusMapper.selectStatusIds(organizationId, statusSearchVO));
        List<StatusWithInfoVO> statusWithInfoVOList = new ArrayList<>();
        if (!statusIdsPage.getContent().isEmpty()) {
            List<StatusWithInfoDTO> statuses = statusMapper.queryStatusList(organizationId, statusIdsPage.getContent());
            statusWithInfoVOList = modelMapper.map(statuses, new TypeToken<List<StatusWithInfoVO>>() {
            }.getType());
        }
        return PageUtil.buildPageInfoWithPageInfoList(statusIdsPage, statusWithInfoVOList);
    }

    @Override
    public StatusVO create(Long organizationId, StatusVO statusVO) {
        if (checkName(organizationId, statusVO.getName()).getStatusExist()) {
            throw new CommonException("error.statusName.exist");
        }
        if (!EnumUtil.contain(StatusType.class, statusVO.getType())) {
            throw new CommonException("error.status.type.illegal");
        }
        statusVO.setOrganizationId(organizationId);
        StatusDTO status = modelMapper.map(statusVO, StatusDTO.class);
        List<StatusDTO> select = statusMapper.select(status);
        if (select.isEmpty()) {
            int isInsert = statusMapper.insert(status);
            if (isInsert != 1) {
                throw new CommonException("error.status.create");
            }
        } else {
            status = select.get(0);
        }
        status = statusMapper.queryById(organizationId, status.getId());
        return modelMapper.map(status, StatusVO.class);
    }

    private Boolean checkNameUpdate(Long organizationId, Long statusId, String name) {
        StatusDTO status = new StatusDTO();
        status.setOrganizationId(organizationId);
        status.setName(name);
        StatusDTO res = statusMapper.selectOne(status);
        return res != null && !statusId.equals(res.getId());
    }

    @Override
    public StatusVO update(StatusVO statusVO) {
        if (checkNameUpdate(statusVO.getOrganizationId(), statusVO.getId(), statusVO.getName())) {
            throw new CommonException("error.statusName.exist");
        }
        if (!EnumUtil.contain(StatusType.class, statusVO.getType())) {
            throw new CommonException("error.status.type.illegal");
        }
        StatusDTO status = modelMapper.map(statusVO, StatusDTO.class);
        int isUpdate = statusMapper.updateByPrimaryKeySelective(status);
        if (isUpdate != 1) {
            throw new CommonException("error.status.update");
        }
        status = statusMapper.queryById(status.getOrganizationId(), status.getId());
        return modelMapper.map(status, StatusVO.class);
    }

    @Override
    public Boolean delete(Long organizationId, Long statusId) {
        StatusDTO status = statusMapper.queryById(organizationId, statusId);
        if (status == null) {
            throw new CommonException("error.status.delete.nofound");
        }
        Long draftUsed = nodeDraftMapper.checkStateDelete(organizationId, statusId);
        Long deployUsed = nodeDeployMapper.checkStateDelete(organizationId, statusId);
        if (draftUsed != 0 || deployUsed != 0) {
            throw new CommonException("error.status.delete");
        }
        if (status.getCode() != null) {
            throw new CommonException("error.status.illegal");
        }
        int isDelete = statusMapper.deleteByPrimaryKey(statusId);
        if (isDelete != 1) {
            throw new CommonException("error.status.delete");
        }
        return true;
    }

    @Override
    public StatusVO queryStatusById(Long organizationId, Long stateId) {
        StatusDTO status = statusMapper.queryById(organizationId, stateId);
        if (status == null) {
            throw new CommonException("error.queryStatusById.notExist");
        }
        return modelMapper.map(status, StatusVO.class);
    }

    @Override
    public StatusVO queryProjectStatusById(Long projectId, Long statusId) {
        Long organizationId = ConvertUtil.getOrganizationId(projectId);
        StatusDTO example = new StatusDTO();
        example.setOrganizationId(organizationId);
        example.setId(statusId);
        StatusDTO result = statusMapper.selectOne(example);
        AssertUtilsForCommonException.notNull(result, "error.status.not.existed");
        ProjectVO projectVO = baseFeignClient.queryProject(projectId).getBody();
        AssertUtilsForCommonException.notNull(projectVO, "error.project.not.existed");
        String applyType =
                ProjectCategory.checkContainProjectCategory(projectVO.getCategories(),ProjectCategory.MODULE_PROGRAM) ? "program" : "agile";
        Long schemeId = projectConfigService.queryById(projectId).getStateMachineSchemeMap().get(applyType).getId();
        Set<Long> issueTypeIdSet=
                nodeDeployMapper.countIssueTypeByStatusIds(organizationId, schemeId, Arrays.asList(statusId), applyType)
                        .stream()
                        .map(IssueCountDTO::getIssueTypeId)
                        .collect(Collectors.toSet());
        StatusVO statusVO = modelMapper.map(result, StatusVO.class);
        statusVO.setIssueTypeIds(new ArrayList<>(issueTypeIdSet));
        IssueStatusDTO issueStatusDTO = issueStatusMapper.selectByStatusId(projectId, statusId);
        boolean completed = false;
        Long issueStatusId = null;
        Long issueStatusObjectVersionNumber = null;
        if (issueStatusDTO != null) {
            completed = issueStatusDTO.getCompleted();
            issueStatusId = issueStatusDTO.getId();
            issueStatusObjectVersionNumber = issueStatusDTO.getObjectVersionNumber();
        }
        statusVO.setIssueStatusId(issueStatusId);
        statusVO.setCompleted(completed);
        statusVO.setIssueStatusObjectVersionNumberId(issueStatusObjectVersionNumber);
        return statusVO;
    }

    @Override
    public Page<StatusVO> queryUserProjectStatus(PageRequest pageRequest, Long organizationId, String type, String param) {
        List<Long> projectIds = new ArrayList<>();
        List<ProjectVO> projects = new ArrayList<>();
        Long userId = DetailsHelper.getUserDetails().getUserId();
        issueService.queryUserProjects(organizationId, null, projectIds, projects, userId, type);
        if (CollectionUtils.isEmpty(projectIds)) {
            return new Page<>();
        }
        List<ProjectConfigDTO> projectConfigs =
                projectConfigMapper.queryByProjectIdsAndOptions(new ArrayList<>(projectIds), SchemeType.STATE_MACHINE, "agile");
        if (Objects.equals(type, "myStarBeacon")) {
            projectConfigs.addAll(projectConfigMapper.queryByProjectIdsAndOptions(new ArrayList<>(projectIds), SchemeType.STATE_MACHINE, "program"));
        }
        if (projectConfigs.isEmpty()) {
            return new Page<>();
        }
        Set<Long> stateMachineSchemeIds =
                projectConfigs
                        .stream()
                        .map(ProjectConfigDTO::getSchemeId)
                        .collect(Collectors.toSet());
        List<Long> stateMachineIds = stateMachineSchemeConfigService.queryBySchemeIds(false, organizationId, stateMachineSchemeIds)
                .stream().map(StatusMachineSchemeConfigVO::getStateMachineId).collect(Collectors.toList());
        return pagedQueryByStateMachineIds(pageRequest, organizationId, stateMachineIds, param);
    }

    @Override
    public List<StatusVO> queryAllStatus(Long organizationId) {
        StatusDTO status = new StatusDTO();
        status.setOrganizationId(organizationId);
        List<StatusDTO> statuses = statusMapper.select(status);
        return modelMapper.map(statuses, new TypeToken<List<StatusVO>>() {
        }.getType());
    }

    @Override
    public Map<Long, StatusVO> queryAllStatusMap(Long organizationId) {
        StatusDTO status = new StatusDTO();
        status.setOrganizationId(organizationId);
        List<StatusDTO> statuses = statusMapper.select(status);
        Map<Long, StatusVO> statusMap = new HashMap<>();
        for (StatusDTO sta : statuses) {
            StatusVO statusVO = modelMapper.map(sta, new TypeToken<StatusVO>() {
            }.getType());
            statusMap.put(statusVO.getId(), statusVO);
        }
        return statusMap;
    }

    @Override
    public StatusCheckVO checkName(Long organizationId, String name) {
        StatusDTO status = new StatusDTO();
        status.setOrganizationId(organizationId);
        status.setName(name);
        StatusDTO res = statusMapper.selectOne(status);
        StatusCheckVO statusCheckVO = new StatusCheckVO();
        if (res != null) {
            statusCheckVO.setStatusExist(true);
            statusCheckVO.setId(res.getId());
            statusCheckVO.setName(res.getName());
            statusCheckVO.setType(res.getType());
        } else {
            statusCheckVO.setStatusExist(false);
        }
        return statusCheckVO;
    }

    @Override
    public StatusCheckVO projectCheckName(Long projectId, Long organizationId, String name) {
        StatusCheckVO statusCheckVO = checkName(organizationId, name);
        if (Boolean.TRUE.equals(statusCheckVO.getStatusExist())) {
            statusCheckVO.setExistIssueTypeVO(projectConfigService.checkExistStatusIssueType(projectId,organizationId,statusCheckVO.getId()));
        }
        return statusCheckVO;
    }

    @Override
    public Map<Long, StatusDTO> batchStatusGet(List<Long> ids) {
        if (!ids.isEmpty()) {
            List<StatusDTO> statuses = statusMapper.batchStatusGet(ids);
            Map<Long, StatusDTO> map = new HashMap();
            for (StatusDTO status : statuses) {
                map.put(status.getId(), status);
            }
            return map;
        } else {
            return new HashMap<>();
        }

    }

    @Override
    public StatusVO createStatusForAgile(Long organizationId, Long stateMachineId, StatusVO statusVO) {
        if (stateMachineId == null) {
            throw new CommonException("error.stateMachineId.notNull");
        }
        if (statusMachineMapper.queryById(organizationId, stateMachineId) == null) {
            throw new CommonException("error.stateMachine.notFound");
        }

        String statusName = statusVO.getName();
        StatusDTO select = new StatusDTO();
        select.setName(statusName);
        select.setOrganizationId(organizationId);
        List<StatusDTO> list = statusMapper.select(select);
        if (list.isEmpty()) {
            statusVO = create(organizationId, statusVO);
        } else {
            statusVO = modelMapper.map(list.get(0), StatusVO.class);
        }
        //将状态加入状态机中，直接加到发布表中
        nodeService.createNodeAndTransformForAgile(organizationId, stateMachineId, statusVO);
        //清理状态机实例
        instanceCache.cleanStateMachine(stateMachineId);
        return statusVO;
    }

    @Override
    public void removeStatusForAgile(Long organizationId, Long stateMachineId, Long statusId) {
        if (statusId == null) {
            throw new CommonException("error.statusId.notNull");
        }
        StatusMachineNodeDTO stateNode = new StatusMachineNodeDTO();
        stateNode.setOrganizationId(organizationId);
        stateNode.setStateMachineId(stateMachineId);
        stateNode.setStatusId(statusId);
        StatusMachineNodeDTO res = nodeDeployMapper.selectOne(stateNode);
        if (res == null) {
            throw new RemoveStatusException("error.status.exist");
        }
        if (res.getType().equals(NodeType.INIT)) {
            throw new RemoveStatusException("error.status.illegal");
        }
        if (res.getId() != null) {
            //删除节点
            nodeDeployMapper.deleteByPrimaryKey(res.getId());
            //删除节点关联的转换
            transformDeployMapper.deleteByNodeId(res.getId());
            //删除节点
            nodeDraftMapper.deleteByPrimaryKey(res.getId());
            //删除节点关联的转换
            transformDraftMapper.deleteByNodeId(res.getId());
        }
        //清理状态机实例
        instanceCache.cleanStateMachine(stateMachineId);
    }

    @Override
    public List<StatusVO> queryByStateMachineIds(Long organizationId, List<Long> stateMachineIds) {
        if (!stateMachineIds.isEmpty()) {
            List<StatusDTO> statuses = statusMapper.queryByStateMachineIds(organizationId, stateMachineIds);
            return modelMapper.map(statuses, new TypeToken<List<StatusVO>>() {
            }.getType());
        }
        return Collections.emptyList();
    }

    @Override
    public Page<StatusVO> pagedQueryByStateMachineIds(PageRequest pageRequest,
                                                      Long organizationId,
                                                      List<Long> stateMachineIds,
                                                      String param) {
        if (ObjectUtils.isEmpty(stateMachineIds)) {
            return PageUtil.emptyPageInfo(pageRequest.getPage(), pageRequest.getSize());
        }
        Page<StatusDTO> status =
                PageHelper.doPageAndSort(pageRequest, () -> statusMapper.queryByStateMachineIdsAndParam(organizationId, stateMachineIds, param));
        List<StatusVO> statusList =
                modelMapper.map(status.getContent(), new TypeToken<List<StatusVO>>() {
        }.getType());
        PageInfo pageInfo = new PageInfo(pageRequest.getPage(), pageRequest.getSize());
        return new Page(statusList, pageInfo, status.getTotalElements());
    }

    @Override
    public Page<ProjectStatusVO> listStatusByProjectId(Long projectId, PageRequest pageRequest, StatusSearchVO statusSearchVO) {
        ProjectVO projectVO = baseFeignClient.queryProject(projectId).getBody();
        Page<ProjectStatusVO> page = PageHelper.doPageAndSort(pageRequest, () -> statusMapper.listStatusByProjectId(projectId, projectVO.getOrganizationId(), statusSearchVO));
        List<ProjectStatusVO> content = page.getContent();
        if (CollectionUtils.isEmpty(content)) {
            return new Page<>();
        }
        List<Long> statusIds = content.stream().map(ProjectStatusVO::getId).collect(Collectors.toList());
        // 查询状态在当前项目的状态机的使用情况
        String applyType = ProjectCategory.checkContainProjectCategory(projectVO.getCategories(),ProjectCategory.MODULE_PROGRAM) ? "program" : "agile";
        ProjectConfigDetailVO projectConfigDetailVO = projectConfigService.queryById(projectId);
        StateMachineSchemeVO stateMachineSchemeVO = projectConfigDetailVO.getStateMachineSchemeMap().get(applyType);
        List<IssueCountDTO> countDTOS = nodeDeployMapper.countIssueTypeByStatusIds(projectVO.getOrganizationId(),stateMachineSchemeVO.getId(),statusIds,applyType);
        Map<Long, List<String>> map = new HashMap<>();
        if (!CollectionUtils.isEmpty(countDTOS)) {
            map.putAll(countDTOS.stream().collect(Collectors.groupingBy(IssueCountDTO::getId, Collectors.mapping(IssueCountDTO::getName, Collectors.toList()))));
        }
        content.forEach(v -> v.setUsage(CollectionUtils.isEmpty(map.get(v.getId())) ? null : StringUtils.collectionToDelimitedString(map.get(v.getId()), ",")));
        page.setContent(content);
        return page;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void deleteStatus(Long projectId, Long statusId, String applyType, List<DeleteStatusTransferVO> statusTransferVOS) {
        // 删掉对应问题类型状态机里面的节点和转换
        projectConfigService.handlerDeleteStatusByProject(projectId, applyType, statusId, statusTransferVOS);
        // 解除状态和项目的关联
        IssueStatusDTO issueStatusDTO = new IssueStatusDTO();
        issueStatusDTO.setStatusId(statusId);
        issueStatusDTO.setProjectId(projectId);
        IssueStatusDTO issueStatus = issueStatusMapper.selectOne(issueStatusDTO);
        if (!ObjectUtils.isEmpty(issueStatus)) {
            issueStatusService.delete(issueStatusDTO);
        }
    }

    @Override
    public Map<String, Object> checkDeleteStatus(Long projectId,String applyType, Long statusId) {
        Map<String, Object> result = new HashMap<>();
        result.put("checkResult", true);
        try {
            // 校验是不是初始状态
            checkInitStatus(projectId,applyType,statusId);
            // 检查对应问题类型状态机里面的节点和转换
            projectConfigService.checkDeleteStatusByProject(projectId, applyType, statusId);
        }catch (Exception e){
            result.put("checkResult", false);
            result.put("errorMsg", MessageAccessor.getMessage(e.getMessage()).getDesc());
        }
        return result;
    }

    @Override
    public List<Long> filterIssueType(Long projectId,String applyType){
        List<Long> filterIssueType = new ArrayList<>();
        filterIssueType.add(0L);
        if (!Objects.equals(applyType, SchemeApplyType.PROGRAM)) {
            Long organizationId = ConvertUtil.getOrganizationId(projectId);
            Long newProjectId = projectId == null ? 0L : projectId;
            IssueTypeSearchVO issueTypeSearchVO = new IssueTypeSearchVO();
            issueTypeSearchVO.setEnabled(true);
            issueTypeMapper.selectByOptions(organizationId, newProjectId, issueTypeSearchVO)
                    .forEach(x -> {
                        if (Objects.equals(x.getTypeCode(), "feature")) {
                            filterIssueType.add(x.getId());
                        }
                    });
        }
        return filterIssueType;
    }

    private void checkInitStatus(Long projectId, String applyType, Long statusId) {
        Long organizationId = ConvertUtil.getOrganizationId(projectId);
        ProjectConfigDetailVO projectConfigDetailVO = projectConfigService.queryById(projectId);
        StateMachineSchemeVO stateMachineSchemeVO = projectConfigDetailVO.getStateMachineSchemeMap().get(applyType);
        List<Long> filterIssueType = filterIssueType(projectId, applyType);
        List<StatusMachineNodeDTO> list = nodeDeployMapper.selectInitNode(organizationId, stateMachineSchemeVO.getId(), statusId, filterIssueType);
        if (!CollectionUtils.isEmpty(list)) {
            throw new CommonException("error.delete.init.status");
        }
    }

    @Override
    public List<StatusAndTransformVO> queryStatusByStateMachineId(Long organizationId, Long projectId, Long stateMachineId) {
        return statusMapper.queryByStateMachineId(organizationId, projectId, stateMachineId);
    }
}
