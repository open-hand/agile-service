package io.choerodon.agile.app.service.impl;

import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.app.service.*;
import io.choerodon.agile.infra.dto.IssueTypeExtendDTO;
import io.choerodon.agile.infra.dto.StatusLinkageExecutionLogDTO;
import io.choerodon.agile.infra.dto.business.IssueDTO;
import io.choerodon.agile.infra.dto.StatusLinkageDTO;
import io.choerodon.agile.infra.dto.StatusMachineTransformDTO;
import io.choerodon.agile.infra.enums.TriggerExecutionStatus;
import io.choerodon.agile.infra.mapper.*;
import io.choerodon.agile.infra.utils.ConvertUtil;
import io.choerodon.agile.infra.utils.SpringBeanUtil;
import io.choerodon.core.exception.CommonException;
import org.apache.commons.lang.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
import org.modelmapper.ModelMapper;
import org.modelmapper.TypeToken;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;

import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * @author zhaotianxin
 * @date 2020-08-17 19:17
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class StatusLinkageServiceImpl implements StatusLinkageService {
    @Autowired
    private StatusLinkageMapper statusLinkageMapper;

    @Autowired
    private ModelMapper modelMapper;

    @Autowired
    private ProjectConfigService projectConfigService;

    @Autowired
    private IssueMapper issueMapper;

    @Autowired
    private IssueService issueService;

    @Autowired
    private StatusMachineTransformMapper statusMachineTransformMapper;
    @Autowired
    private IssueTypeMapper issueTypeMapper;

    @Autowired
    private OrganizationConfigService organizationConfigService;

    @Autowired
    private StatusService statusService;

    @Autowired
    private IssueTypeService issueTypeService;

    @Autowired
    private LinkIssueStatusLinkageMapper linkIssueStatusLinkageMapper;

    @Autowired
    private StatusLinkageService statusLinkageService;

    @Autowired
    private StatusLinkageExecutionLogService statusLinkageExecutionLogService;

    @Autowired
    private StatusTransferSettingService statusTransferSettingService;

    @Override
    public List<StatusLinkageVO> createOrUpdate(Long projectId, Long issueTypeId, Long statusId, Long objectVersionNumber, String applyType, List<StatusLinkageVO> linkageVOS) {
        List<StatusLinkageDTO> statusLinkageDTOS = queryByStatusIdAndIssueTypeId(projectId, issueTypeId, statusId);
        if (!CollectionUtils.isEmpty(statusLinkageDTOS)) {
            deleteByStatusIdAndIssueTypeId(projectId, issueTypeId, statusId);
        }
        if (!CollectionUtils.isEmpty(linkageVOS)) {
            for (StatusLinkageVO statusLinkageVO : linkageVOS) {
                StatusLinkageDTO statusLinkageDTO = modelMapper.map(statusLinkageVO, StatusLinkageDTO.class);
                statusLinkageDTO.setProjectId(projectId);
                statusLinkageDTO.setIssueTypeId(issueTypeId);
                statusLinkageDTO.setStatusId(statusId);
                statusLinkageDTO.setParentIssueTypeCode("");
                baseInsert(statusLinkageDTO);
            }
        }
        projectConfigService.updateNodeObjectVersionNumber(projectId, issueTypeId, statusId, objectVersionNumber, applyType);
        return listByIssueTypeAndStatusId(projectId, issueTypeId, statusId);
    }

    private void baseInsert(StatusLinkageDTO statusLinkageDTO) {
        if (statusLinkageMapper.insertSelective(statusLinkageDTO) != 1) {
            throw new CommonException("error.insert.status.linkage");
        }
    }

    private void deleteByStatusIdAndIssueTypeId(Long projectId, Long issueTypeId, Long statusId) {
        StatusLinkageDTO statusLinkageDTO = new StatusLinkageDTO();
        statusLinkageDTO.setProjectId(projectId);
        statusLinkageDTO.setIssueTypeId(issueTypeId);
        statusLinkageDTO.setStatusId(statusId);
        statusLinkageMapper.delete(statusLinkageDTO);
    }

    private List<StatusLinkageDTO> queryByStatusIdAndIssueTypeId(Long projectId, Long issueTypeId, Long statusId) {
        StatusLinkageDTO statusLinkageDTO = new StatusLinkageDTO();
        statusLinkageDTO.setProjectId(projectId);
        statusLinkageDTO.setIssueTypeId(issueTypeId);
        statusLinkageDTO.setStatusId(statusId);
        return statusLinkageMapper.select(statusLinkageDTO);
    }

    @Override
    public List<StatusLinkageVO> listByIssueTypeAndStatusId(Long projectId, Long issueTypeId, Long statusId) {
        List<StatusLinkageDTO> statusLinkageDTOS = queryByStatusIdAndIssueTypeId(projectId, issueTypeId, statusId);
        if (CollectionUtils.isEmpty(statusLinkageDTOS)) {
            return new ArrayList<>();
        }
        return modelMapper.map(statusLinkageDTOS, new TypeToken<List<StatusLinkageVO>>() {
        }.getType());
    }

    @Override
    public List<StatusLinkageVO> listByStatusIds(Long projectId, Long issueTypeId, List<Long> statusIds, String applyType) {
        List<StatusLinkageDTO> statusLinkageDTOS = statusLinkageMapper.selectByStatusIds(projectId, issueTypeId, statusIds);
        if (CollectionUtils.isEmpty(statusLinkageDTOS)) {
            return new ArrayList<>();
        }
        List<StatusLinkageVO> linkageVOS = modelMapper.map(statusLinkageDTOS, new TypeToken<List<StatusLinkageVO>>() {
        }.getType());
        // 获取项目的状态
        List<StatusVO> statusVOS = projectConfigService.queryStatusByProjectId(projectId, applyType);
        Map<Long, StatusVO> statusMap = new HashMap<>();
        if (!CollectionUtils.isEmpty(statusVOS)) {
            statusMap.putAll(statusVOS.stream().collect(Collectors.toMap(StatusVO::getId, Function.identity())));
        }
        // 获取项目的问题类型
        List<IssueTypeVO> issueTypeVOS = projectConfigService.queryIssueTypesByProjectId(projectId, applyType, false);
        Map<Long, IssueTypeVO> typeVOMap = new HashMap<>();
        if (!CollectionUtils.isEmpty(statusVOS)) {
            typeVOMap.putAll(issueTypeVOS.stream().collect(Collectors.toMap(IssueTypeVO::getId, Function.identity())));
        }
        Set<Long> issueTypeIds = new HashSet<>();
        Set<Long> projectIds = new HashSet<>();
        for (StatusLinkageVO statusLinkageVO : linkageVOS) {
            statusLinkageVO.setStatusVO(statusMap.get(statusLinkageVO.getParentIssueStatusSetting()));
            statusLinkageVO.setIssueTypeVO(typeVOMap.get(statusLinkageVO.getParentIssueTypeId()));
            issueTypeIds.add(statusLinkageVO.getIssueTypeId());
            projectIds.add(statusLinkageVO.getProjectId());
        }
        Map<Long, Map<Long, String>> map = new HashMap<>();
        final Long zero = 0L;
        Long organizationId = ConvertUtil.getOrganizationId(projectId);
        issueTypeMapper.selectWithAliasByIds(issueTypeIds, projectIds, organizationId)
                .forEach(x -> {
                    Long id = x.getId();
                    Map<Long, String> projectIssueTypeMap = map.computeIfAbsent(id, y -> new HashMap<>());
                    projectIssueTypeMap.put(zero, x.getName());
                    List<IssueTypeExtendDTO> issueTypeExtends = x.getIssueTypeExtends();
                    if (!ObjectUtils.isEmpty(issueTypeExtends)) {
                        issueTypeExtends.forEach(y -> projectIssueTypeMap.put(y.getProjectId(), y.getName()));
                    }
                });
        linkageVOS.forEach(x -> {
            Long id = x.getIssueTypeId();
            Map<Long, String> projectIssueTypeMap = map.get(id);
            Long thisProjectId = x.getProjectId();
            String name = projectIssueTypeMap.get(thisProjectId);
            if (name == null) {
                name = projectIssueTypeMap.get(zero);
            }
            x.setIssueTypeName(name);
        });
        return linkageVOS;
    }

    @Override
    public boolean updateParentStatus(Long projectId,
                                      Long issueId,
                                      String applyType,
                                      Set<Long> influenceIssueIds) {
        IssueDTO issueDTO = issueMapper.selectByPrimaryKey(issueId);
        if (ObjectUtils.isEmpty(issueDTO)) {
            throw new CommonException("error.issue.null");
        }
        AgilePluginService agilePluginService = SpringBeanUtil.getExpandBean(AgilePluginService.class);
        if (agilePluginService != null) {
            agilePluginService.storyLinkageFeature(projectId,issueDTO,applyType);
        }
        // 判断issue是不是子任务或者子bug
        Boolean checkBugOrSubTask = checkIsSubBugOrSubTask(issueDTO);
        if (Boolean.FALSE.equals(checkBugOrSubTask)) {
            return true;
        }
        List<StatusLinkageDTO> statusLinkageDTOS = queryByStatusIdAndIssueTypeId(projectId, issueDTO.getIssueTypeId(), issueDTO.getStatusId());
        if (CollectionUtils.isEmpty(statusLinkageDTOS)) {
            return true;
        }
        Map<Long, StatusLinkageDTO> statusLinkageDTOMap = statusLinkageDTOS.stream().collect(Collectors.toMap(StatusLinkageDTO::getParentIssueTypeId, Function.identity()));
        Long parentIssueId = getParentIssueId(issueDTO);
        IssueDTO parentIssue = issueMapper.selectByPrimaryKey(parentIssueId);
        StatusLinkageDTO statusLinkageDTO = statusLinkageDTOMap.get(parentIssue.getIssueTypeId());
        if (ObjectUtils.isEmpty(statusLinkageDTO)) {
            return true;
        }
        // 统计子任务的状态
        Boolean isChange = false;
        Long changeStatus = null;
        // 查询父任务的子任务
        List<IssueDTO> issueDTOS = issueMapper.querySubIssueByParentIssueId(projectId, parentIssueId);
        List<Long> issueTypeIds = issueDTOS.stream().map(IssueDTO::getIssueTypeId).collect(Collectors.toList());
        List<StatusLinkageDTO> select = statusLinkageMapper.listByIssueTypeIdsParentTypeId(projectId,parentIssue.getIssueTypeId(),issueTypeIds,statusLinkageDTO.getParentIssueStatusSetting());
        Map<Long, List<StatusLinkageDTO>> linkageDTOMap = select.stream().collect(Collectors.groupingBy(StatusLinkageDTO::getIssueTypeId));
        Map<Long, List<IssueDTO>> issueMap = issueDTOS.stream().collect(Collectors.groupingBy(IssueDTO::getIssueTypeId));
        if (select.size() == 1 && statusLinkageDTO.getIssueTypeId().equals(issueDTO.getIssueTypeId())) {
            isChange = handlerSingleIssueType(Arrays.asList(statusLinkageDTO), issueMap, issueDTO.getIssueTypeId());
            changeStatus = getChangeStatus(isChange, statusLinkageDTO);
        } else {
            Map<String, Object> variables = new HashMap<>();
            handlerMultiSetting(variables, select, issueDTO, issueMap, linkageDTOMap, issueDTOS);
            isChange = BooleanUtils.toBoolean(variables.get("isChange").toString());
            Object statusId = variables.get("changeStatus");
            changeStatus = !ObjectUtils.isEmpty(statusId) ? Long.valueOf(statusId.toString()) : null;
        }
        // 判断是否改变父任务的状态
        if (Boolean.TRUE.equals(isChange)) {
            //收集受影响的issueId
            influenceIssueIds.add(parentIssueId);
            if (Objects.equals(changeStatus, parentIssue.getStatusId())) {
                statusLinkageExecutionLog(projectId, statusLinkageDTO.getId(), parentIssue.getIssueId(), issueDTO, TriggerExecutionStatus.STOP.getValue(), "same_status");
                return true;
            }
            if (statusTransferSettingService.verifyStatusTransferSetting(projectId, issueDTO, changeStatus)) {
                statusLinkageExecutionLog(projectId, statusLinkageDTO.getId(), parentIssue.getIssueId(), issueDTO, TriggerExecutionStatus.STOP.getValue(), "condition_limit");
                return true;
            }
            boolean result = changeParentStatus(projectId, applyType, parentIssue, changeStatus, issueDTO);
            statusLinkageExecutionLog(projectId, statusLinkageDTO.getId(), parentIssue.getIssueId(), issueDTO, TriggerExecutionStatus.SUCCESS.getValue(), null);
            return result;
        }
        return true;
    }

    private void statusLinkageExecutionLog(Long projectId, Long linkSettingId, Long issueId, IssueDTO influenceIssue, String statusCode, String remark) {
        LinkIssueStatusLinkageVO linkIssueStatusLinkageVO = statusLinkageService.queryById(projectId, linkSettingId);
        if (ObjectUtils.isEmpty(linkIssueStatusLinkageVO)) {
            throw new CommonException("error.link.issue.status.linkage.empty");
        }
        // 记录联动的执行日志
        String content = issueService.buildStatusLinkageContent(linkIssueStatusLinkageVO);
        StatusLinkageExecutionLogDTO statusLinkageExecutionLogDTO = new StatusLinkageExecutionLogDTO();
        statusLinkageExecutionLogDTO.setPreIssueId(influenceIssue.getIssueId());
        statusLinkageExecutionLogDTO.setCurIssueId(issueId);
        statusLinkageExecutionLogDTO.setContent(content);
        statusLinkageExecutionLogDTO.setRemark(remark);
        statusLinkageExecutionLogDTO.setStatusCode(statusCode);
        statusLinkageExecutionLogService.create(projectId, ConvertUtil.getOrganizationId(projectId), statusLinkageExecutionLogDTO);
    }

    @Override
    public void getUpdateParentStatusIssue(Long projectId, IssueDTO issue, Long statusId, String applyType, InfluenceIssueVO influenceIssueVO, Map<Long, List<Long>> allInfluenceMap, Map<Long, List<IssueLinkChangeVO>> issueLinkChangeGroup) {
        if (ObjectUtils.isEmpty(issue)) {
            throw new CommonException("error.issue.null");
        }
        Long issueId = issue.getIssueId();
        // 判断issue是不是子任务或者子bug
        Boolean checkBugOrSubTask = checkIsSubBugOrSubTask(issue);
        if (Boolean.FALSE.equals(checkBugOrSubTask)) {
            return;
        }

        List<StatusLinkageDTO> statusLinkageDTOS = queryByStatusIdAndIssueTypeId(projectId, issue.getIssueTypeId(), issue.getStatusId());
        if (CollectionUtils.isEmpty(statusLinkageDTOS)) {
            return;
        }
        Map<Long, StatusLinkageDTO> statusLinkageDTOMap = statusLinkageDTOS.stream().collect(Collectors.toMap(StatusLinkageDTO::getParentIssueTypeId, Function.identity()));
        Long parentIssueId = getParentIssueId(issue);
        IssueDTO parentIssue = issueMapper.selectByPrimaryKey(parentIssueId);
        StatusLinkageDTO statusLinkageDTO = statusLinkageDTOMap.get(parentIssue.getIssueTypeId());
        if (ObjectUtils.isEmpty(statusLinkageDTO)) {
            return;
        }
        // 统计子任务的状态
        Boolean isChange = false;
        Long changeStatus = null;
        // 查询父任务的子任务
        List<IssueDTO> issueDTOS = issueMapper.querySubIssueByParentIssueId(projectId, parentIssueId);
        for (IssueDTO dto : issueDTOS) {
            List<Long> influenceStatusIds = allInfluenceMap.getOrDefault(dto.getIssueId(), new ArrayList<>());
            if (!CollectionUtils.isEmpty(influenceStatusIds)) {
                dto.setStatusId(influenceStatusIds.get(influenceStatusIds.size() -1));
            }
        }
        List<Long> issueTypeIds = issueDTOS.stream().map(IssueDTO::getIssueTypeId).collect(Collectors.toList());
        List<StatusLinkageDTO> select = statusLinkageMapper.listByIssueTypeIdsParentTypeId(projectId,parentIssue.getIssueTypeId(),issueTypeIds,statusLinkageDTO.getParentIssueStatusSetting());
        Map<Long, List<StatusLinkageDTO>> linkageDTOMap = select.stream().collect(Collectors.groupingBy(StatusLinkageDTO::getIssueTypeId));
        Map<Long, List<IssueDTO>> issueMap = issueDTOS.stream().collect(Collectors.groupingBy(IssueDTO::getIssueTypeId));
        if (select.size() == 1 && statusLinkageDTO.getIssueTypeId().equals(issue.getIssueTypeId())) {
            isChange = handlerSingleIssueType(Arrays.asList(statusLinkageDTO), issueMap, issue.getIssueTypeId());
            changeStatus = getChangeStatus(isChange, statusLinkageDTO);
        } else {
            Map<String, Object> variables = new HashMap<>();
            handlerMultiSetting(variables, select, issue, issueMap, linkageDTOMap, issueDTOS);
            isChange = BooleanUtils.toBoolean(variables.get("isChange").toString());
            Object changeStatus1 = variables.get("changeStatus");
            changeStatus = !ObjectUtils.isEmpty(changeStatus1) ? Long.valueOf(changeStatus1.toString()) : null;
        }
        if (isChange) {
            List<Long> statusIds = allInfluenceMap.getOrDefault(parentIssueId, new ArrayList<>());
            InfluenceIssueVO influenceIssue = new InfluenceIssueVO();
            influenceIssue.setIssueId(parentIssueId);
            influenceIssue.setStatusId(changeStatus);
            influenceIssue.setLoop(false);
            influenceIssue.setLinkSettingId(statusLinkageDTO.getId());
            influenceIssue.setChildrenTriggered(true);
            if(statusIds.contains(changeStatus)){
                allInfluenceMap.put(0L, new ArrayList<>());
                influenceIssue.setLoop(true);
                return;
            }
            issueService.handlerInfluenceMap(allInfluenceMap, parentIssueId, changeStatus, issueLinkChangeGroup, issueId, influenceIssue, false);
            List<InfluenceIssueVO> childrenVOS = influenceIssueVO.getChildrenVO();
            if(CollectionUtils.isEmpty(childrenVOS)){
                childrenVOS = new ArrayList<>();
            }
            childrenVOS.add(influenceIssue);
            influenceIssueVO.setChildrenVO(childrenVOS);
        }

    }

    @Override
    public List<StatusLinkageVO> listStatusLinkageByProjectId(Long projectId) {
        List<StatusLinkageVO> allStatusLinkage = new ArrayList<>();
        List<StatusLinkageVO> linkageVOS = statusLinkageMapper.selectWithStatusByProjectId(projectId);
        Map<Long, List<Long>> map = new HashMap<>();
        if (!CollectionUtils.isEmpty(linkageVOS)) {
            allStatusLinkage.addAll(linkageVOS);
            map.putAll(linkageVOS.stream().collect(Collectors.groupingBy(StatusLinkageVO::getIssueTypeId, Collectors.mapping(StatusLinkageVO::getStatusId, Collectors.toList()))));
        }
        List<StatusLinkageVO> statusLinkageVOS = linkIssueStatusLinkageMapper.selectWithStatusByProjectId(projectId);
        if (!CollectionUtils.isEmpty(statusLinkageVOS)) {
            for (StatusLinkageVO statusLinkageVO : statusLinkageVOS) {
                List<Long> existStatusIds = map.getOrDefault(statusLinkageVO.getIssueTypeId(), new ArrayList<>());
                if (!existStatusIds.contains(statusLinkageVO.getStatusId())) {
                    existStatusIds.add(statusLinkageVO.getStatusId());
                    map.put(statusLinkageVO.getIssueTypeId(), existStatusIds);
                    allStatusLinkage.add(statusLinkageVO);
                }
            }
        }
        return allStatusLinkage;
    }

    @Override
    public List<StatusLinkageVO> saveStatusLinkage(Long organizationId, Long issueTypeId, Long statusId, Long objectVersionNumber, List<StatusLinkageVO> linkageVOS) {
        List<StatusLinkageDTO> statusLinkageDTOS = queryByOrg(organizationId, issueTypeId, statusId);
        if (!CollectionUtils.isEmpty(statusLinkageDTOS)) {
            StatusLinkageDTO statusLinkageDTO = new StatusLinkageDTO();
            statusLinkageDTO.setProjectId(0L);
            statusLinkageDTO.setOrganizationId(organizationId);
            statusLinkageDTO.setIssueTypeId(issueTypeId);
            statusLinkageDTO.setStatusId(statusId);
            statusLinkageMapper.delete(statusLinkageDTO);
        }
        if (!CollectionUtils.isEmpty(linkageVOS)) {
            for (StatusLinkageVO statusLinkageVO : linkageVOS) {
                StatusLinkageDTO statusLinkageDTO = modelMapper.map(statusLinkageVO, StatusLinkageDTO.class);
                statusLinkageDTO.setProjectId(0L);
                statusLinkageDTO.setIssueTypeId(issueTypeId);
                statusLinkageDTO.setStatusId(statusId);
                statusLinkageDTO.setOrganizationId(organizationId);
                statusLinkageDTO.setParentIssueTypeCode("");
                baseInsert(statusLinkageDTO);
            }
        }
        organizationConfigService.updateNodeObjectVersionNumber(organizationId, issueTypeId, statusId, objectVersionNumber);
        return listByOptions(organizationId, issueTypeId, statusId);
    }
    private List<StatusLinkageDTO> queryByOrg(Long organizationId, Long issueTypeId, Long statusId){
        StatusLinkageDTO statusLinkageDTO = new StatusLinkageDTO();
        statusLinkageDTO.setProjectId(0L);
        statusLinkageDTO.setOrganizationId(organizationId);
        statusLinkageDTO.setIssueTypeId(issueTypeId);
        statusLinkageDTO.setStatusId(statusId);
        return statusLinkageMapper.select(statusLinkageDTO);
    }

    @Override
    public List<StatusLinkageVO> listByOptions(Long organizationId, Long issueTypeId, Long statusId) {
        List<StatusLinkageDTO> statusLinkageDTOS = queryByOrg(organizationId, issueTypeId, statusId);
        if (CollectionUtils.isEmpty(statusLinkageDTOS)) {
            return new ArrayList<>();
        }
        return modelMapper.map(statusLinkageDTOS, new TypeToken<List<StatusLinkageVO>>() {
        }.getType());
    }

    @Override
    public List<StatusLinkageVO> listStatusLinkage(Long organizationId, Long issueTypeId, List<Long> statusIds) {
        List<StatusLinkageDTO> statusLinkageDTOS = statusLinkageMapper.listOptions(organizationId, issueTypeId, statusIds);
        if (CollectionUtils.isEmpty(statusLinkageDTOS)) {
            return new ArrayList<>();
        }
        List<StatusLinkageVO> linkageVOS = modelMapper.map(statusLinkageDTOS, new TypeToken<List<StatusLinkageVO>>() {
        }.getType());
        // 获取项目的状态
        Map<Long, StatusVO> statusVOMap = statusService.queryAllStatusMap(organizationId);
        if (ObjectUtils.isEmpty(statusVOMap)) {
            statusVOMap = new HashMap<>();
        }
        // 获取项目的问题类型
        List<IssueTypeVO> issueTypeVOS = issueTypeService.queryByOrgId(organizationId, 0L);
        Map<Long, IssueTypeVO> typeVOMap = new HashMap<>();
        if (!CollectionUtils.isEmpty(statusVOMap)) {
            typeVOMap.putAll(issueTypeVOS.stream().collect(Collectors.toMap(IssueTypeVO::getId, Function.identity())));
        }
        for (StatusLinkageVO statusLinkageVO : linkageVOS) {
            statusLinkageVO.setStatusVO(statusVOMap.get(statusLinkageVO.getParentIssueStatusSetting()));
            statusLinkageVO.setIssueTypeVO(typeVOMap.get(statusLinkageVO.getParentIssueTypeId()));
        }
        return linkageVOS;
    }

    @Override
    public LinkIssueStatusLinkageVO queryById(Long projectId, Long id) {
        StatusLinkageDTO statusLinkageDTO = statusLinkageMapper.selectByPrimaryKey(id);
        if (ObjectUtils.isEmpty(statusLinkageDTO)) {
            return null;
        }
        LinkIssueStatusLinkageVO linkIssueStatusLinkageVO = new LinkIssueStatusLinkageVO();
        Long organizationId = ConvertUtil.getOrganizationId(projectId);
        // 获取项目的状态
        Map<Long, StatusVO> statusVOMap = statusService.queryAllStatusMap(organizationId);
        if (ObjectUtils.isEmpty(statusVOMap)) {
            statusVOMap = new HashMap<>();
        }
        // 获取项目的问题类型
        List<IssueTypeVO> issueTypeVOS = issueTypeService.queryByOrgId(organizationId, 0L);
        Map<Long, IssueTypeVO> typeVOMap = new HashMap<>();
        if (!CollectionUtils.isEmpty(statusVOMap)) {
            typeVOMap.putAll(issueTypeVOS.stream().collect(Collectors.toMap(IssueTypeVO::getId, Function.identity())));
        }
        linkIssueStatusLinkageVO.setIssueTypeVO(typeVOMap.getOrDefault(statusLinkageDTO.getIssueTypeId(), null));
        linkIssueStatusLinkageVO.setStatusVO(statusVOMap.getOrDefault(statusLinkageDTO.getStatusId(), null));
        linkIssueStatusLinkageVO.setLinkIssueType(typeVOMap.getOrDefault(statusLinkageDTO.getParentIssueTypeId(), null));
        linkIssueStatusLinkageVO.setLinkIssueStatus(statusVOMap.getOrDefault(statusLinkageDTO.getParentIssueStatusSetting(), null));
        IssueLinkTypeVO issueLinkTypeVO = new IssueLinkTypeVO();
        issueLinkTypeVO.setLinkName("父级");
        linkIssueStatusLinkageVO.setLinkTypeVO(issueLinkTypeVO);
        return linkIssueStatusLinkageVO;
    }

    protected boolean changeParentStatus(Long projectId, String applyType, IssueDTO parentIssue, Long changeStatus, IssueDTO triggerIssue) {
        if (parentIssue.getStatusId().equals(changeStatus)) {
            return true;
        }
        // 获取当前状态对应的transformId
        Long stateMachineId = projectConfigService.queryStateMachineId(projectId, applyType, parentIssue.getIssueTypeId());
        // 获取开始状态和结束状态查询转换Id
        Long organizationId = ConvertUtil.getOrganizationId(projectId);
        List<StatusMachineTransformDTO> statusMachineTransformDTOS = statusMachineTransformMapper
                .selectTransformByStatusId(organizationId, stateMachineId, parentIssue.getStatusId(), changeStatus, false);
        if (CollectionUtils.isEmpty(statusMachineTransformDTOS)){
            statusMachineTransformDTOS = statusMachineTransformMapper
                    .selectTransformByStatusId(organizationId, stateMachineId, parentIssue.getStatusId(), changeStatus, true);
        }
        if (CollectionUtils.isEmpty(statusMachineTransformDTOS)) {
            return !StringUtils.equals("agile", applyType);
        }
        StatusMachineTransformDTO statusTransform = statusMachineTransformDTOS.get(0);
        issueService.updateIssueStatus(projectId, parentIssue.getIssueId(), statusTransform.getId(),
                parentIssue.getObjectVersionNumber(), applyType, triggerIssue, true);
        return true;
    }

    private void handlerMultiSetting(Map<String, Object> variables, List<StatusLinkageDTO> select, IssueDTO issueDTO, Map<Long, List<IssueDTO>> issueMap, Map<Long, List<StatusLinkageDTO>> linkageDTOMap, List<IssueDTO> issueDTOS) {
        // 判断两种子任务的设置的父级状态是不是相同
        Boolean isChange = false;
        Long changeStatus = null;
        Set<Long> ids = select.stream().map(StatusLinkageDTO::getParentIssueStatusSetting).collect(Collectors.toSet());
        if (ids.size() > 1) {
            List<StatusLinkageDTO> statusLinkageDTOS = linkageDTOMap.get(issueDTO.getIssueTypeId());
            isChange = handlerSingleIssueType(statusLinkageDTOS, issueMap, issueDTO.getIssueTypeId());
            changeStatus = statusLinkageDTOS.get(0).getParentIssueStatusSetting();
        } else {
            Map<Long, String> typeCodeMap = issueDTOS.stream().collect(Collectors.toMap(IssueDTO::getIssueTypeId, IssueDTO::getTypeCode,(code1,code2) -> code1));
            Iterator<Map.Entry<Long, List<StatusLinkageDTO>>> iterator = linkageDTOMap.entrySet().iterator();
            while (iterator.hasNext()) {
                Map.Entry<Long, List<StatusLinkageDTO>> next = iterator.next();
                List<StatusLinkageDTO> value = next.getValue();
                isChange = handlerSingleIssueType(value, issueMap, next.getKey());
                if (Boolean.FALSE.equals(isChange)) {
                    break;
                }
                changeStatus = ids.iterator().next();
            }
        }
        variables.put("isChange", isChange);
        variables.put("changeStatus", changeStatus);
    }

    private Long getParentIssueId(IssueDTO issueDTO) {
        return "bug".equals(issueDTO.getTypeCode()) ? issueDTO.getRelateIssueId() : issueDTO.getParentIssueId();
    }

    private Long getChangeStatus(Boolean isChange, StatusLinkageDTO statusLink) {
        return Boolean.TRUE.equals(isChange) ? statusLink.getParentIssueStatusSetting() : null;
    }

    private boolean checkIsSubBugOrSubTask(IssueDTO issueDTO) {
        Boolean isSubTask = "sub_task".equals(issueDTO.getTypeCode());
        Boolean isSubBug = "bug".equals(issueDTO.getTypeCode()) && (!ObjectUtils.isEmpty(issueDTO.getRelateIssueId()) && issueDTO.getRelateIssueId() != 0);
        if (isSubTask || isSubBug) {
            return Boolean.TRUE;
        }
        return Boolean.FALSE;
    }

    private Boolean handlerSingleIssueType(List<StatusLinkageDTO> statusLinks, Map<Long, List<IssueDTO>> issueMap, Long issueTypeId) {
        List<IssueDTO> sub = issueMap.get(issueTypeId);
        if (CollectionUtils.isEmpty(sub)) {
            return Boolean.TRUE;
        }
        List<Long> statusLinkStatus = statusLinks.stream().map(StatusLinkageDTO::getStatusId).collect(Collectors.toList());
        long count = sub.stream().filter(v -> statusLinkStatus.contains(v.getStatusId())).count();
        if (Boolean.FALSE.equals((count == sub.size()))) {
            return Boolean.FALSE;
        }
        return Boolean.TRUE;
    }
}
