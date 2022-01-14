package io.choerodon.agile.app.service.impl;


import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.api.vo.event.TransformInfo;
import io.choerodon.agile.app.service.*;
import io.choerodon.agile.infra.cache.InstanceCache;
import io.choerodon.agile.infra.dto.*;
import io.choerodon.agile.infra.dto.business.IssueDTO;
import io.choerodon.agile.infra.enums.*;
import io.choerodon.agile.infra.exception.RemoveStatusException;
import io.choerodon.agile.infra.feign.BaseFeignClient;
import io.choerodon.agile.infra.feign.operator.TestServiceClientOperator;
import io.choerodon.agile.infra.feign.vo.ProjectCategoryDTO;
import io.choerodon.agile.infra.mapper.*;
import io.choerodon.agile.infra.utils.*;
import io.choerodon.core.domain.Page;
import io.choerodon.core.exception.CommonException;
import io.choerodon.mybatis.pagehelper.PageHelper;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.hzero.core.base.BaseConstants;
import org.hzero.mybatis.domian.Condition;
import org.hzero.mybatis.util.Sqls;
import org.modelmapper.ModelMapper;
import org.modelmapper.TypeToken;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.cloud.context.config.annotation.RefreshScope;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.Assert;
import org.springframework.util.ObjectUtils;

import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;


/**
 * @author shinan.chen
 * @Date 2018/10/24
 */
@Service
@RefreshScope
@Transactional(rollbackFor = Exception.class)
public class ProjectConfigServiceImpl implements ProjectConfigService {

    private static final String AGILE_SERVICE = "agile-service";
    private static final String FLAG = "flag";
    private static final String MESSAGE = "message";
    private static final String STATEMACHINEID = "stateMachineId";
    private static final String ERROR_ISSUE_STATE_MACHINE_NOT_FOUND = "error.issueStateMachine.notFound";
    private static final String ERROR_ISSUE_STATUS_NOT_FOUND = "error.createIssue.issueStatusNotFound";
    private static final String ERROR_APPLYTYPE_ILLEGAL = "error.applyType.illegal";
    private static final String ERROR_STATEMACHINESCHEMEID_NULL = "error.stateMachineSchemeId.null";
    private static final String FEATURE_TYPE_CODE = "feature";

    @Autowired
    private ProjectConfigMapper projectConfigMapper;
    @Autowired
    private IssueTypeMapper issueTypeMapper;
    @Autowired
    private StateMachineSchemeConfigService stateMachineSchemeConfigService;
    @Autowired
    private IssueTypeSchemeService issueTypeSchemeService;
    @Autowired
    private StateMachineSchemeService stateMachineSchemeService;
    @Autowired
    private ProjectUtil projectUtil;
    @Autowired
    private ProjectConfigService projectConfigService;
    @Autowired
    private IssueStatusMapper issueStatusMapper;
    @Autowired
    private StatusService statusService;
    @Autowired
    private InstanceService instanceService;
    @Autowired
    private StateMachineTransformService transformService;
    @Autowired
    private ColumnStatusRelMapper columnStatusRelMapper;
    @Autowired
    private ModelMapper modelMapper;
    @Autowired
    private StatusMachineNodeMapper statusMachineNodeMapper;
    @Autowired
    private StateMachineNodeService stateMachineNodeService;
    @Autowired
    private StatusMachineTransformMapper statusMachineTransformMapper;
    @Autowired
    private IssueMapper issueMapper;
    @Autowired
    private InstanceCache instanceCache;
    @Autowired
    private IssueStatusService issueStatusService;
    @Autowired
    private StatusMapper statusMapper;
    @Autowired
    private StatusTransferSettingService statusTransferSettingService;
    @Autowired
    private IssueAccessDataService issueAccessDataService;
    @Autowired
    private StatusFieldSettingService statusFieldSettingService;
    @Autowired
    private StatusNoticeSettingService statusNoticeSettingService;
    @Autowired
    private StatusLinkageService statusLinkageService;
    @Autowired
    private StatusLinkageMapper statusLinkageMapper;
    @Autowired
    private BoardColumnMapper boardColumnMapper;
    @Autowired
    private StatusNoticeSettingMapper statusNoticeSettingMapper;
    @Autowired
    private StatusTransferSettingMapper statusTransferSettingMapper;
    @Autowired
    private StatusFieldSettingMapper statusFieldSettingMapper;
    @Autowired
    private IssueTypeSchemeConfigMapper issueTypeSchemeConfigMapper;
    @Autowired
    private BaseFeignClient baseFeignClient;
    @Autowired
    private StatusMachineNodeMapper nodeDeployMapper;
    @Autowired
    private StatusBranchMergeSettingService statusBranchMergeSettingService;
    @Autowired
    private TestServiceClientOperator testServiceClientOperator;
    @Autowired
    private StatusBranchMergeSettingMapper statusBranchMergeSettingMapper;
    
    @Autowired
    private LinkIssueStatusLinkageService linkIssueStatusLinkageService;
    @Autowired
    private StatusMachineSchemeConfigMapper statusMachineSchemeConfigMapper;

    @Override
    public ProjectConfigDTO create(Long projectId, Long schemeId, String schemeType, String applyType) {
        if (Boolean.FALSE.equals(EnumUtil.contain(SchemeType.class, schemeType))) {
            throw new CommonException("error.schemeType.illegal");
        }
        if (Boolean.FALSE.equals(EnumUtil.contain(SchemeApplyType.class, applyType))) {
            throw new CommonException(ERROR_APPLYTYPE_ILLEGAL);
        }
        ProjectConfigDTO projectConfig = new ProjectConfigDTO(projectId, schemeId, schemeType, applyType);
        //保证幂等性
        List<ProjectConfigDTO> configs = projectConfigMapper.select(projectConfig);
        if (!configs.isEmpty()) {
            return configs.get(0);
        }
        int result = projectConfigMapper.insert(projectConfig);
        if (result != 1) {
            throw new CommonException("error.projectConfig.create");
        }

        return projectConfig;
    }

    @Override
    public ProjectConfigDetailVO queryById(Long projectId) {
        Long organizationId = projectUtil.getOrganizationId(projectId);
        List<ProjectConfigDTO> projectConfigs = projectConfigMapper.queryByProjectId(projectId);
        Map<String, List<ProjectConfigDTO>> configMap = projectConfigs.stream().collect(Collectors.groupingBy(ProjectConfigDTO::getSchemeType));
        ProjectConfigDetailVO projectConfigDetailVO = new ProjectConfigDetailVO();
        projectConfigDetailVO.setProjectId(projectId);
        //获取问题类型方案
        List<ProjectConfigDTO> issueTypeSchemeConfigs = configMap.get(SchemeType.ISSUE_TYPE);
        if (issueTypeSchemeConfigs != null && !issueTypeSchemeConfigs.isEmpty()) {
            Map<String, IssueTypeSchemeVO> issueTypeSchemeMap = new HashMap<>(issueTypeSchemeConfigs.size());
            for (ProjectConfigDTO projectConfig : issueTypeSchemeConfigs) {
                IssueTypeSchemeVO issueTypeSchemeVO = issueTypeSchemeService.queryById(organizationId, projectId, projectConfig.getSchemeId());
                issueTypeSchemeMap.put(projectConfig.getApplyType(), issueTypeSchemeVO);
            }
            projectConfigDetailVO.setIssueTypeSchemeMap(issueTypeSchemeMap);
        }
        //获取状态机方案
        List<ProjectConfigDTO> stateMachineSchemeConfigs = configMap.get(SchemeType.STATE_MACHINE);
        if (stateMachineSchemeConfigs != null && !stateMachineSchemeConfigs.isEmpty()) {
            Map<String, StateMachineSchemeVO> stateMachineSchemeMap = new HashMap<>(stateMachineSchemeConfigs.size());
            for (ProjectConfigDTO projectConfig : stateMachineSchemeConfigs) {
                StateMachineSchemeVO stateMachineSchemeVO = stateMachineSchemeService.querySchemeWithConfigById(false, organizationId, projectConfig.getSchemeId(), projectId);
                stateMachineSchemeMap.put(projectConfig.getApplyType(), stateMachineSchemeVO);
            }
            projectConfigDetailVO.setStateMachineSchemeMap(stateMachineSchemeMap);
        }
        return projectConfigDetailVO;
    }

    @Override
    public List<IssueTypeVO> queryIssueTypesByProjectId(Long projectId,
                                                        String applyType,
                                                        boolean onlyEnabled) {
        if (Boolean.FALSE.equals(EnumUtil.contain(SchemeApplyType.class, applyType))) {
            throw new CommonException(ERROR_APPLYTYPE_ILLEGAL);
        }
        Long organizationId = ConvertUtil.getOrganizationId(projectId);
        ProjectConfigDTO projectConfig = projectConfigMapper.queryBySchemeTypeAndApplyType(projectId, SchemeType.ISSUE_TYPE, applyType);
        //获取问题类型方案
        if (projectConfig.getSchemeId() != null) {
            //根据方案配置表获取 问题类型
            List<IssueTypeDTO> issueTypes = issueTypeMapper.queryBySchemeId(organizationId, projectId, projectConfig.getSchemeId(), onlyEnabled);
            return modelMapper.map(issueTypes, new TypeToken<List<IssueTypeVO>>() {
            }.getType());
        } else {
            throw new CommonException("error.queryIssueTypesByProjectId.issueTypeSchemeId.null");
        }
    }

    @Override
    public List<IssueTypeWithStateMachineIdVO> queryIssueTypesWithStateMachineIdByProjectId(Long projectId,
                                                                                            String applyType,
                                                                                            Boolean onlyEnabled) {
        // 获取项目的 applyTypes
        List<String> applyTypes = new ArrayList<>();
        if (ObjectUtils.isEmpty(applyType)) {
            applyTypes = ProjectCategory.getProjectApplyType(projectId);
        } else {
            applyTypes.add(applyType);
        }
        Long organizationId = projectUtil.getOrganizationId(projectId);
        //根据 applyTypes 查询所有的问题类型
        List<IssueTypeDTO> issueTypes = issueTypeMapper.queryByApplyTypes(organizationId, projectId, applyTypes, onlyEnabled, SchemeType.ISSUE_TYPE);
        issueTypes = issueTypes.stream().filter(v -> !(Objects.equals(SchemeApplyType.AGILE, v.getApplyType()) && Objects.equals("feature", v.getTypeCode()))).collect(Collectors.toList());
        // 查询 appleTypes 所有的问题类型的状态机
        Map<String, Map<Long, Long>> map = stateMachineSchemeConfigService.queryStatusMachineMapByAppleTypes(organizationId, projectId, applyTypes);
        List<IssueTypeWithStateMachineIdVO> issueTypeWithStateMachineIds = modelMapper.map(issueTypes, new TypeToken<List<IssueTypeWithStateMachineIdVO>>() {
        }.getType());
        issueTypeWithStateMachineIds.forEach(x -> {
            Map<Long, Long> statusMachineMap = map.getOrDefault(x.getApplyType(), new HashMap<>());
            Long stateMachineId = statusMachineMap.get(x.getId());
            if (stateMachineId != null) {
                x.setStateMachineId(stateMachineId);
            } else {
                x.setStateMachineId(statusMachineMap.get(0L));
            }
        });
        return issueTypeWithStateMachineIds;
    }

    @Override
    public List<StatusVO> queryStatusByIssueTypeId(Long projectId, Long issueTypeId, String applyType) {
        Long organizationId = projectUtil.getOrganizationId(projectId);
        Long stateMachineSchemeId = projectConfigMapper.queryBySchemeTypeAndApplyType(projectId, SchemeType.STATE_MACHINE, applyType).getSchemeId();
        List<ProjectStatusVO> projectStatusVOList = statusMapper.listStatusByProjectId(projectId, organizationId,null);
        Set<Long> statusIds = projectStatusVOList.stream().map(ProjectStatusVO::getId).collect(Collectors.toSet());
        if (stateMachineSchemeId == null) {
            throw new CommonException(ERROR_STATEMACHINESCHEMEID_NULL);
        }
        //获取状态机
        Long stateMachineId = stateMachineSchemeConfigService.queryStateMachineIdBySchemeIdAndIssueTypeId(false, organizationId, stateMachineSchemeId, issueTypeId);
        List<StatusAndTransformVO> statuses = statusService.queryStatusByStateMachineId(organizationId, projectId, stateMachineId);
        if(CollectionUtils.isEmpty(statuses)){
            return new ArrayList<>();
        }
        List<StatusAndTransformVO> result = statuses.stream()
                .filter(statusAndTransformVO -> statusIds.contains(statusAndTransformVO.getId())).collect(Collectors.toList());
        return modelMapper.map(result, new TypeToken<List<StatusVO>>() {}.getType());
    }

    @Override
    public List<StatusVO> queryStatusByProjectId(Long projectId, String applyType) {
        List<String> applyTypes = new ArrayList<>();
        if (ObjectUtils.isEmpty(applyType)) {
            applyTypes = ProjectCategory.getProjectApplyType(projectId);
        } else {
            applyTypes.add(applyType);
        }
        List<StatusVO> result = queryStatusByProjectIdNotType(projectId, applyTypes);
        //设置状态关联的issueTypeIds
        List<Long> statusIds = result.stream().map(StatusVO::getId).collect(Collectors.toList());
        ProjectConfigDetailVO projectConfigDetailVO = projectConfigService.queryById(projectId);
        List<Long> schemeIds = new ArrayList<>();
        applyTypes.forEach(v -> {
            StateMachineSchemeVO stateMachineSchemeVO = projectConfigDetailVO.getStateMachineSchemeMap().get(v);
            if (!ObjectUtils.isEmpty(stateMachineSchemeVO)) {
                schemeIds.add(stateMachineSchemeVO.getId());
            }
        });
        Boolean isAgile = !applyTypes.contains(SchemeApplyType.PROGRAM);
        List<IssueCountDTO> issueCounts = nodeDeployMapper.countStatusIssueTypeScope(ConvertUtil.getOrganizationId(projectId), schemeIds, statusIds, isAgile);
        Map<Long, List<Long>> map = new HashMap<>();
        if (!CollectionUtils.isEmpty(issueCounts)) {
            map.putAll(issueCounts.stream().collect(Collectors.groupingBy(IssueCountDTO::getId, Collectors.mapping(IssueCountDTO::getIssueTypeId, Collectors.toList()))));
        }
        result.forEach(status -> status.setIssueTypeIds(map.get(status.getId())));
        return result;
    }

    @Override
    public List<StatusVO> queryStatusByProjectIdNotType(Long projectId, List<String> applyTypes) {
        Long organizationId = ConvertUtil.getOrganizationId(projectId);
        ConvertUtil.getOrganizationId(projectId);
        List<StatusMachineSchemeConfigVO> list = statusMachineSchemeConfigMapper.queryStatusMachineMapByAppleTypes(organizationId, projectId, applyTypes);
        List<Long> filterIssueType = statusService.filterIssueType(projectId, applyTypes);
        if(CollectionUtils.isEmpty(list)){
            return new ArrayList<>();
        }
        List<Long> stateMachineIds = new ArrayList<>();
        for (StatusMachineSchemeConfigVO statusMachineSchemeConfigVO : list) {
            Boolean isAgileFeature = Objects.equals(SchemeApplyType.AGILE, statusMachineSchemeConfigVO.getApplyType()) && filterIssueType.contains(statusMachineSchemeConfigVO.getApplyType());
            if (Objects.equals(0L, statusMachineSchemeConfigVO.getIssueTypeId()) || isAgileFeature) {
                continue;
            }
            stateMachineIds.add(statusMachineSchemeConfigVO.getStateMachineId());
        }
        return statusService.queryByStateMachineIds(organizationId, stateMachineIds);
    }

    @Override
    public List<TransformVO> queryTransformsByProjectId(Long projectId, Long currentStatusId, Long issueId, Long issueTypeId, String applyType) {
        if (Boolean.FALSE.equals(EnumUtil.contain(SchemeApplyType.class, applyType))) {
            throw new CommonException(ERROR_APPLYTYPE_ILLEGAL);
        }
        Long organizationId = projectUtil.getOrganizationId(projectId);
        ProjectConfigDTO projectConfig = projectConfigMapper.queryBySchemeTypeAndApplyType(projectId, SchemeType.STATE_MACHINE, applyType);
        //获取状态机方案
        if (projectConfig.getSchemeId() != null) {
            //获取状态机
            Long stateMachineId = stateMachineSchemeConfigService.queryStateMachineIdBySchemeIdAndIssueTypeId(false, organizationId, projectConfig.getSchemeId(), issueTypeId);
            //获取当前状态拥有的转换
            List<TransformInfo> transformInfos = instanceService.queryListTransform(organizationId, AGILE_SERVICE, stateMachineId, issueId, currentStatusId);
            List<TransformVO> transformVOS = modelMapper.map(transformInfos, new TypeToken<List<TransformVO>>() {
            }.getType());
            //获取组织中所有状态
            List<StatusVO> statusVOS = statusService.queryAllStatus(organizationId);
            Map<Long, StatusVO> statusMap = statusVOS.stream().collect(Collectors.toMap(StatusVO::getId, x -> x));
            // 查询哪些状态能够流转
            List<Long> statusIds = transformVOS.stream().map(TransformVO::getEndStatusId).collect(Collectors.toList());
            List<Long> canTransformStatus = statusTransferSettingService.checkStatusTransform(projectId,issueTypeId, statusIds);
            List<TransformVO> collect = transformVOS.stream().filter(v -> canTransformStatus.contains(v.getEndStatusId())).collect(Collectors.toList());
            collect.forEach(transformVO -> {
                StatusVO statusVO = statusMap.get(transformVO.getEndStatusId());
                transformVO.setStatusVO(statusVO);
            });
            //如果转换中不包含当前状态，则添加一个self
            if (collect.stream().noneMatch(transformVO -> currentStatusId.equals(transformVO.getEndStatusId()))) {
                TransformVO self = new TransformVO();
                self.setEndStatusId(currentStatusId);
                self.setStatusVO(statusMap.get(currentStatusId));
                transformVOS.add(self);
            }
            return collect;
        } else {
            throw new CommonException("error.queryIssueTypesByProjectId.stateMachineSchemeId.null");
        }
    }

    @Override
    public Map<Long, Map<Long, List<TransformVO>>> queryTransformsMapByProjectId(Long projectId,Long boardId,String applyType) {
        if (Boolean.FALSE.equals(EnumUtil.contain(SchemeApplyType.class, applyType))) {
            throw new CommonException(ERROR_APPLYTYPE_ILLEGAL);
        }
        //获取状态机方案
        Long organizationId = projectUtil.getOrganizationId(projectId);
        ProjectConfigDTO smProjectConfig = projectConfigMapper.queryBySchemeTypeAndApplyType(projectId, SchemeType.STATE_MACHINE, applyType);
        if (smProjectConfig.getSchemeId() == null) {
            throw new CommonException("error.queryTransformsMapByProjectId.stateMachineSchemeId.null");
        }
        List<StatusMachineSchemeConfigVO> smsConfigVO = stateMachineSchemeConfigService.queryBySchemeId(false, organizationId, smProjectConfig.getSchemeId());
        //获取问题类型方案
        ProjectConfigDTO itProjectConfig = projectConfigMapper.queryBySchemeTypeAndApplyType(projectId, SchemeType.ISSUE_TYPE, applyType);
        if (itProjectConfig.getSchemeId() == null) {
            throw new CommonException("error.queryTransformsMapByProjectId.issueTypeSchemeId.null");
        }
        List<IssueTypeDTO> issueTypes = issueTypeMapper.queryBySchemeId(organizationId, projectId, itProjectConfig.getSchemeId(), false);
        // 史诗和普通项目群的特性类型没必要在查询转换
        List<Long> skipIssueTypeId = new ArrayList<>();
        skipIssueTypeId.add(0L);
        issueTypes.forEach(v -> {
            boolean isSkip = "issue_epic".equals(v.getTypeCode()) || (SchemeApplyType.AGILE.equals(applyType) && FEATURE_TYPE_CODE.equals(v.getTypeCode()));
            if (Boolean.TRUE.equals(isSkip)) {
                skipIssueTypeId.add(v.getId());
            }
        });
        List<Long> stateMachineIds = smsConfigVO.stream().filter(v -> !skipIssueTypeId.contains(v.getIssueTypeId())).map(StatusMachineSchemeConfigVO::getStateMachineId).collect(Collectors.toList());
        //状态机id->状态id->转换列表
        Map<Long, Map<Long, List<TransformVO>>> statusMap = transformService.queryStatusTransformsMap(organizationId, stateMachineIds);
        Map<Long, Long> idMap = smsConfigVO.stream().collect(Collectors.toMap(StatusMachineSchemeConfigVO::getIssueTypeId, StatusMachineSchemeConfigVO::getStateMachineId));
        //问题类型id->状态id->转换列表
        Map<Long, Map<Long, List<TransformVO>>> resultMap = new HashMap<>(issueTypes.size());
        //获取组织所有状态
        List<StatusVO> statusVOS = statusService.queryAllStatus(organizationId);
        Map<Long, StatusVO> sMap = statusVOS.stream().collect(Collectors.toMap(StatusVO::getId, x -> x));
        //匹配默认状态机的问题类型映射
        Set<Long> boardStatus = new HashSet<>();
        if (!ObjectUtils.isEmpty(boardId)) {
            // 查询出面板上有的状态
            boardStatus.addAll(boardColumnMapper.queryStatusByBoardId(projectId,boardId));
        }
        //匹配状态机的问题类型映射
        for (IssueTypeDTO issueType : issueTypes) {
            boolean isSkip = "issue_epic".equals(issueType.getTypeCode()) || (SchemeApplyType.AGILE.equals(applyType) && FEATURE_TYPE_CODE.equals(issueType.getTypeCode()));
            if (Boolean.TRUE.equals(isSkip)) {
                continue;
            }
            Long stateMachineId = idMap.get(issueType.getId());
            if (stateMachineId != null) {
                Map<Long, List<TransformVO>> statusTransferMap = statusMap.get(stateMachineId);
                Set<Long> allStatus = new HashSet<>();
                allStatus.addAll(!ObjectUtils.isEmpty(boardId) ? boardStatus : statusTransferMap.keySet());
                // 查询能转换的状态
                List<Long> canTransferStatus = statusTransferSettingService.checkStatusTransform(projectId, issueType.getId(), new ArrayList<>(allStatus));
                // 过滤掉不能转换的状态
                Map<Long, List<TransformVO>> transferMap = new HashMap<>();
                statusTransferMap.entrySet().stream().filter(entry -> entry.getKey() != 0L && boardStatus.contains(entry.getKey())).forEach(entry ->
                    transferMap.put(entry.getKey(),entry.getValue().stream().filter(v ->  canTransferStatus.contains(v.getEndStatusId())).map(v -> {
                        StatusVO statusVO = sMap.get(v.getEndStatusId());
                        if (statusVO != null) {
                            v.setStatusType(statusVO.getType());
                        }
                        return v;
                    }).collect(Collectors.toList())));
                resultMap.put(issueType.getId(),transferMap);
            }
        }
        return resultMap;
    }

    @Override
    public List<StatusAndTransformVO> statusTransformList(Long projectId, Long issueTypeId, String applyType) {
        if (Boolean.FALSE.equals(EnumUtil.contain(SchemeApplyType.class, applyType))) {
            throw new CommonException(ERROR_APPLYTYPE_ILLEGAL);
        }
        // 获取状态加Id
        Long stateMachineId = queryStateMachineIdAndCheck(projectId, applyType, issueTypeId);
        Long organizationId = ConvertUtil.getOrganizationId(projectId);
        // 处理rank值
        stateMachineNodeService.handlerNullRankNode(organizationId, stateMachineId, applyType);
        // 查询当前状态机有哪些状态
        List<StatusAndTransformVO> statusVOS = statusService.queryStatusByStateMachineId(organizationId, projectId, stateMachineId);
        if (CollectionUtils.isEmpty(statusVOS)) {
            return new ArrayList<>();
        }
        List<Long> issueUseStatusId = issueMapper.selectStatusIdByIssueType(projectId,issueTypeId);
        //状态机id->状态id->转换列表
        Map<Long, Map<Long, List<TransformVO>>> statusMap = transformService.queryStatusTransformsMap(organizationId, Arrays.asList(stateMachineId));
        Map<Long, List<TransformVO>> listMap = statusMap.get(stateMachineId);
        for (StatusAndTransformVO item : statusVOS) {
            List<TransformVO> transformVOS = listMap.get(item.getId());
            if (!CollectionUtils.isEmpty(transformVOS)) {
                item.setCanTransformStatus(transformVOS.stream().map(TransformVO::getEndStatusId).collect(Collectors.toSet()));
            }
            item.setHasIssue(issueUseStatusId.contains(item.getId()));
        }

        return statusVOS;
    }


    @Override
    public void defaultStatus(Long projectId, Long issueTypeId, Long stateMachineId, Long statusId) {
        Long organizationId = ConvertUtil.getOrganizationId(projectId);
        StatusMachineNodeDTO statusMachineNodeDTO = new StatusMachineNodeDTO(stateMachineId,statusId,organizationId);
        StatusMachineNodeDTO nodeDTO = statusMachineNodeMapper.selectOne(statusMachineNodeDTO);
        if (ObjectUtils.isEmpty(nodeDTO)) {
            throw new CommonException("error.state.machine.node.null");
        }
        // 查询状态机的原默认状态
        statusMachineNodeDTO.setType(NodeType.INIT);
        statusMachineNodeDTO.setStatusId(null);
        List<StatusMachineNodeDTO> nodes = statusMachineNodeMapper.select(statusMachineNodeDTO);
        if (nodes.isEmpty()) {
            throw new CommonException("error.queryInitStatusId.notFound");
        }
        StatusMachineNodeDTO olderDefaultNode = nodes.get(0);
        if (Objects.equals(olderDefaultNode.getStatusId(), statusId)) {
            return;
        }
        // 更新
        olderDefaultNode.setType(NodeType.CUSTOM);
        stateMachineNodeService.baseUpdate(olderDefaultNode);
        nodeDTO.setType(NodeType.INIT);
        // 设置rank值
        String minRank = statusMachineNodeMapper.queryMinRank(organizationId, stateMachineId);
        if (!ObjectUtils.isEmpty(minRank)) {
            nodeDTO.setRank(RankUtil.genPre(minRank));
        }
        stateMachineNodeService.baseUpdate(nodeDTO);
        // 修改初始转换
        changeInitTransform(organizationId, stateMachineId, olderDefaultNode.getId(), nodeDTO.getId());
        //清理状态机实例
        instanceCache.cleanStateMachine(stateMachineId);
    }

    private void changeInitTransform(Long organizationId, Long stateMachineId, Long oldNodeId, Long newNodeId) {
        if (Objects.equals(oldNodeId, newNodeId)) {
            return;
        }
        StatusMachineTransformDTO initTransform = new StatusMachineTransformDTO();
        initTransform.setStateMachineId(stateMachineId);
        initTransform.setEndNodeId(oldNodeId);
        initTransform.setType("transform_init");
        initTransform.setOrganizationId(organizationId);
        StatusMachineTransformDTO oldTransform = statusMachineTransformMapper.selectOne(initTransform);
        if (oldTransform == null) {
            throw new CommonException("error.statusMachine.initTransform.notFound");
        }
        oldTransform.setEndNodeId(newNodeId);
        if (statusMachineTransformMapper.updateByPrimaryKeySelective(oldTransform) != 1) {
            throw new CommonException("error.initTransform.update");
        }
    }

    @Override
    public List<StateMachineTransformUpdateVO> updateTransformByIssueTypeId(Long projectId, Long issueTypeId, String applyType, List<StateMachineTransformUpdateVO> list) {
        Long stateMachine = queryStateMachineIdAndCheck(projectId, applyType, issueTypeId);
        if (CollectionUtils.isEmpty(list)) {
            throw new CommonException("error.transform.null");
        }
        Long organizationId = ConvertUtil.getOrganizationId(projectId);
        for (StateMachineTransformUpdateVO transformUpdateVO : list) {
            if (ObjectUtils.isEmpty(transformUpdateVO.getEndNodeId()) || ObjectUtils.isEmpty(transformUpdateVO.getStartNodeId())) {
                throw new CommonException("error.node.id.null");
            }
            if (Boolean.TRUE.equals(transformUpdateVO.getSelect())) {
                transformService.createTransform(organizationId, stateMachine, transformUpdateVO);
            } else {
                transformService.deleteTransformByNodeId(organizationId, stateMachine, transformUpdateVO.getStartNodeId(), transformUpdateVO.getEndNodeId());
            }
        }
        //清理状态机实例
        instanceCache.cleanStateMachine(stateMachine);
        return list;
    }

    @Override
    public StatusVO createStatus(Long projectId, List<Long> issueTypeIds, String applyType, StatusVO statusVO) {
        if (ObjectUtils.isEmpty(statusVO.getName()) && ObjectUtils.isEmpty(statusVO.getType())) {
            throw new CommonException("error.status.name.or.type.null");
        }
        Long organizationId = ConvertUtil.getOrganizationId(projectId);
        StatusCheckVO statusCheckVO = statusService.checkName(organizationId, statusVO.getName());
        // 创建状态
        StatusVO status;
        if (Boolean.TRUE.equals(statusCheckVO.getStatusExist())) {
            StatusDTO statusInDb = statusMapper.queryById(organizationId, statusCheckVO.getId());
            status = modelMapper.map(statusInDb, StatusVO.class);
        }else {
            status = statusService.create(organizationId, statusVO);
        }
        // 关联状态机
        if (!CollectionUtils.isEmpty(issueTypeIds)) {
            List<IssueTypeWithStateMachineIdVO> issueTypes = queryIssueTypesWithStateMachineIdByProjectId(projectId, null, false);
            Map<Long, String> issueTypeApplyTypeMap = issueTypes.stream().collect(Collectors.toMap(IssueTypeWithStateMachineIdVO::getId, IssueTypeWithStateMachineIdVO::getApplyType));
            statusVO.setId(status.getId());
            for (Long issueTypeId:issueTypeIds) {
                linkStatus(projectId, issueTypeId, issueTypeApplyTypeMap.get(issueTypeId), statusVO);
            }
        }
        return status;
    }

    @Override
    public StatusMachineNodeVO linkStatus(Long projectId,
                                          Long issueTypeId,
                                          String applyType,
                                          StatusVO status) {
        if (Objects.isNull(applyType)) {
            applyType = getApplyType(projectId, issueTypeId);
        }
        Long statusId = status.getId();
        Boolean defaultStatus = status.getDefaultStatus();
        Boolean transferAll = status.getTransferAll();
        boolean completed = Boolean.TRUE.equals(status.getCompleted());
        Long organizationId = ConvertUtil.getOrganizationId(projectId);
        IssueStatusDTO issueStatusDTO = issueStatusMapper.selectByStatusId(projectId, statusId);
        StatusVO statusVO = statusService.queryStatusById(organizationId, statusId);
        if (ObjectUtils.isEmpty(issueStatusDTO)) {
            issueStatusDTO = new IssueStatusDTO();
            issueStatusDTO.setProjectId(projectId);
            issueStatusDTO.setStatusId(statusId);
            issueStatusDTO.setCompleted(completed);
            issueStatusDTO.setName(statusVO.getName());
            issueStatusDTO.setCategoryCode(statusVO.getType());
            issueStatusDTO.setEnable(false);
            issueStatusService.insertIssueStatus(issueStatusDTO);
        }
        Long stateMachineId = queryStateMachineIdAndCheck(projectId, applyType, issueTypeId);
        stateMachineNodeService.handlerNullRankNode(organizationId, stateMachineId, applyType);
        StatusMachineNodeDTO stateMachineNode = new StatusMachineNodeDTO();
        stateMachineNode.setStatusId(statusId);
        stateMachineNode.setOrganizationId(organizationId);
        stateMachineNode.setStateMachineId(stateMachineId);
        // 校验是否已存在关联的状态
        List<StatusMachineNodeDTO> select = statusMachineNodeMapper.select(stateMachineNode);
        if (CollectionUtils.isEmpty(select)) {
            List<StatusMachineNodeVO> statusMachineNodeVOS = stateMachineNodeService.queryByStateMachineId(organizationId, stateMachineId, false);
            stateMachineNode.setType(NodeType.CUSTOM);
            String maxRank = statusMachineNodeMapper.queryMaxRank(organizationId, stateMachineId);
            stateMachineNode.setRank(ObjectUtils.isEmpty(maxRank) ? RankUtil.mid() : RankUtil.genNext(maxRank));
            stateMachineNodeService.baseCreate(stateMachineNode);
            if (Boolean.TRUE.equals(defaultStatus)) {
                defaultStatus(projectId, issueTypeId, stateMachineId, statusId);
            }
            // 默认可以全部流转到当前状态(设置为)
            if (ObjectUtils.isEmpty(transferAll) || Boolean.TRUE.equals(transferAll)) {
                transformAll(statusMachineNodeVOS, organizationId, statusId, stateMachineId, stateMachineNode.getId());
            }
            else {
                String nodeName = statusVO.getName() + "转换到" + statusVO.getName();
                StatusMachineTransformDTO statusMachineTransformDTO = new StatusMachineTransformDTO(nodeName, stateMachineId, stateMachineNode.getId(), stateMachineNode.getId(), TransformType.CUSTOM, TransformConditionStrategy.ALL, organizationId);
                statusMachineTransformMapper.batchInsert(Arrays.asList(statusMachineTransformDTO));
            }
        }
        instanceCache.cleanStateMachine(stateMachineId);
        return modelMapper.map(stateMachineNode, StatusMachineNodeVO.class);
    }

    @Override
    public String getApplyType(Long projectId, Long issueTypeId) {
        List<String> applyTypes = ProjectCategory.getProjectApplyType(projectId);
        List<StatusMachineSchemeConfigVO> list = statusMachineSchemeConfigMapper.queryStatusMachineMapByAppleTypes(ConvertUtil.getOrganizationId(projectId), projectId, applyTypes);
        if(CollectionUtils.isEmpty(list)){
            return null;
        }
        Map<Long, String> map = list.stream().filter(v -> !Objects.equals(0L, v.getIssueTypeId())).collect(Collectors.toMap(StatusMachineSchemeConfigVO::getIssueTypeId, StatusMachineSchemeConfigVO::getApplyType));
        return map.getOrDefault(issueTypeId, null);
    }

    @Override
    public void transformAll(List<StatusMachineNodeVO> statusMachineNodeVOS, Long organizationId, Long statusId, Long stateMachineId, Long nodeId) {
        if (!CollectionUtils.isEmpty(statusMachineNodeVOS)) {
            StatusVO statusVO = statusService.queryStatusById(organizationId, statusId);
            List<StatusMachineTransformDTO> list = new ArrayList<>();
            List<StatusMachineNodeVO> collect = statusMachineNodeVOS.stream().filter(v -> !NodeType.START.equals(v.getType())).collect(Collectors.toList());
            for (StatusMachineNodeVO statusMachineNodeVO : collect) {
                String name = statusVO.getName() + "转换到" + statusMachineNodeVO.getStatusVO().getName();
                StatusMachineTransformDTO stateMachineTransform = new StatusMachineTransformDTO(name, stateMachineId, nodeId, statusMachineNodeVO.getId(), TransformType.CUSTOM, TransformConditionStrategy.ALL, organizationId);
                list.add(stateMachineTransform);
                String name1 = statusMachineNodeVO.getStatusVO().getName()  + "转换到" + statusVO.getName();
                StatusMachineTransformDTO statusMachineTransformDTO = new StatusMachineTransformDTO(name1, stateMachineId, statusMachineNodeVO.getId(), nodeId, TransformType.CUSTOM, TransformConditionStrategy.ALL, organizationId);
                list.add(statusMachineTransformDTO);
            }
            // 转换到自身
            String nodeName = statusVO.getName() + "转换到" + statusVO.getName();
            StatusMachineTransformDTO statusMachineTransformDTO = new StatusMachineTransformDTO(nodeName, stateMachineId, nodeId, nodeId, TransformType.CUSTOM, TransformConditionStrategy.ALL, organizationId);
            list.add(statusMachineTransformDTO);
            statusMachineTransformMapper.batchInsert(list);
        }
    }

    @Override
    public void deleteNode(Long projectId, Long issueTypeId, String applyType, Long nodeId, Long statusId) {
        Assert.notNull(projectId, BaseConstants.ErrorCode.DATA_INVALID);
        Long organizationId = ConvertUtil.getOrganizationId(projectId);
        Long stateMachineId = queryStateMachineIdAndCheck(projectId, applyType, issueTypeId);
        StatusMachineNodeDTO statusMachineNodeDTO = new StatusMachineNodeDTO();
        statusMachineNodeDTO.setOrganizationId(organizationId);
        statusMachineNodeDTO.setStateMachineId(stateMachineId);
        StatusMachineNodeDTO currentNode = checkStatusLink(projectId, issueTypeId, nodeId);
        Long currentStatusId = currentNode.getStatusId();
        Assert.notNull(currentStatusId, BaseConstants.ErrorCode.DATA_INVALID);
        // 删除当前node的转换
        statusMachineTransformMapper.deleteByStateMachineIdAndNodeId(organizationId,stateMachineId,nodeId);
        // 删除node
        statusMachineNodeDTO.setStatusId(null);
        statusMachineNodeDTO.setId(nodeId);
        statusMachineNodeMapper.delete(statusMachineNodeDTO);
        // 检测状态是否与其他node有关联
        if (!statusMachineNodeMapper.existByProjectId(projectId, currentStatusId,applyType)){
            // 无关联则删除与issue_status关联
            IssueStatusDTO issueStatusDTO = new IssueStatusDTO();
            issueStatusDTO.setProjectId(projectId);
            issueStatusDTO.setStatusId(currentStatusId);
            issueStatusMapper.delete(issueStatusDTO);
        }
        // 校验当前状态是否是项目下最后一个状态，是则删除所有看版列的关系
        List<IssueStatusDTO> issueStatusExist =
                issueStatusMapper.selectByCondition(Condition.builder(IssueStatusDTO.class)
                .andWhere(Sqls.custom().andEqualTo("statusId", currentStatusId)
                        .andEqualTo("projectId", projectId)).build());
        if (CollectionUtils.isEmpty(issueStatusExist)){
            boardColumnMapper.deleteByStatusId(projectId, currentStatusId);
        }

        StatusBranchMergeSettingDTO statusBranchMergeSettingDTO = new StatusBranchMergeSettingDTO();
        statusBranchMergeSettingDTO.setOrganizationId(organizationId);
        statusBranchMergeSettingDTO.setProjectId(projectId);
        statusBranchMergeSettingDTO.setIssueTypeId(issueTypeId);
        statusBranchMergeSettingDTO.setStatusId(statusId);
        statusBranchMergeSettingMapper.delete(statusBranchMergeSettingDTO);

        // 清除状态机实例
        instanceCache.cleanStateMachine(stateMachineId);
    }

    @Override
    public void checkDeleteNode(Long projectId, Long issueTypeId, String applyType, Long nodeId) {
        Assert.notNull(projectId, BaseConstants.ErrorCode.DATA_INVALID);
        Long organizationId = ConvertUtil.getOrganizationId(projectId);
        Long stateMachineId = queryStateMachineIdAndCheck(projectId, applyType, issueTypeId);
        StatusMachineNodeDTO statusMachineNodeDTO = new StatusMachineNodeDTO();
        statusMachineNodeDTO.setOrganizationId(organizationId);
        statusMachineNodeDTO.setStateMachineId(stateMachineId);
        checkStatusLink(projectId, issueTypeId, nodeId);
    }

    @Override
    public List<IssueTypeVO> checkExistStatusIssueType(Long projectId, Long organizationId, Long statusId) {
        List<String> applyTypes = ProjectCategory.getProjectApplyType(projectId);
        List<Long> issueTypeIds = projectConfigMapper.getExistStatusTypeIds(organizationId, projectId, statusId, applyTypes);
        if(CollectionUtils.isEmpty(issueTypeIds)){
            return new ArrayList<>();
        }
        IssueTypeSearchVO issueTypeSearchVO = new IssueTypeSearchVO();
        issueTypeSearchVO.setIssueTypeIds(issueTypeIds);
        return issueTypeMapper.selectByOptions(organizationId, projectId, issueTypeSearchVO);
    }

    @Override
    public NodeSortVO updateSort(Long projectId, Long statusMachineId, NodeSortVO nodeSortVO, String applyType) {
        if (ObjectUtils.isEmpty(nodeSortVO.getNodeId())) {
            throw new CommonException("error.sort.node.null");
        }
        if (ObjectUtils.isEmpty(nodeSortVO.getOutSetId())) {
            throw new CommonException("error.outSetId.null");
        }
        ProjectVO projectVO = baseFeignClient.queryProject(projectId).getBody();
        // 对rank值为空的node进行处理
        stateMachineNodeService.handlerNullRankNode(projectVO.getOrganizationId(), statusMachineId, applyType);
        // 进行排序
        stateMachineNodeService.sortNode(projectVO.getOrganizationId(), statusMachineId, nodeSortVO, applyType);
        // 清除状态机实例
        instanceCache.cleanStateMachine(statusMachineId);
        return null;
    }

    private StatusMachineNodeDTO checkStatusLink(Long projectId, Long issueTypeId, Long nodeId) {
        StatusMachineNodeDTO machineNodeDTO = statusMachineNodeMapper.selectByPrimaryKey(nodeId);
        Assert.notNull(machineNodeDTO, BaseConstants.ErrorCode.DATA_NOT_EXISTS);
        Long currentStatusId = machineNodeDTO.getStatusId();
        if (Objects.equals(issueTypeId,0L)) {
            return machineNodeDTO;
        }
        // 校验是否是初始状态
        if (StringUtils.equals("node_init", machineNodeDTO.getType())) {
            throw new CommonException("error.delete.init.status");
        }
        // 校验当前node的状态有没有被项目下的issue使用
        Boolean checkIssueUse = checkIssueUse(projectId, issueTypeId,machineNodeDTO.getStatusId());
        if (Boolean.TRUE.equals(checkIssueUse)) {
            // 报错
            throw new CommonException("error.status.status_issue_used");
        }
        // 校验当前node的状态是否与其他状态有联动
        IssueTypeDTO issueTypeDTO = issueTypeMapper.selectWithAlias(issueTypeId, projectId);
        List<StatusLinkageDTO> linkExistList =
                statusLinkageMapper.selectExistStatusLink(projectId, machineNodeDTO.getStatusId(), issueTypeDTO.getId());
        if (CollectionUtils.isNotEmpty(linkExistList)){
            throw new CommonException("error.status.status_link_exist");
        }
        Sqls existCondition = Sqls.custom().andEqualTo("projectId", projectId).andEqualTo("statusId", currentStatusId).andEqualTo("issueTypeId", issueTypeId);
        // 校验是否关联流转条件
        List<StatusTransferSettingDTO> transferExist =
                statusTransferSettingMapper.selectByCondition(Condition.builder(StatusTransferSettingDTO.class)
                .andWhere(existCondition).build());
        if (CollectionUtils.isNotEmpty(transferExist)){
            throw new CommonException("error.status.status_transfer_exist");
        }
        // 校验是否关联属性字段
        List<StatusFieldSettingDTO> statusFieldExist =
                statusFieldSettingMapper.selectByCondition(Condition.builder(StatusFieldSettingDTO.class)
                .andWhere(existCondition).build());
        if (CollectionUtils.isNotEmpty(statusFieldExist)){
            throw new CommonException("error.status.status_field_exist");
        }
        // 校验是否存在通知设置
        List<StatusNoticeSettingDTO> statusNoticeExist =
                statusNoticeSettingMapper.selectByCondition(Condition.builder(StatusNoticeSettingDTO.class)
                        .andWhere(existCondition).build());
        if (CollectionUtils.isNotEmpty(statusNoticeExist)){
            throw new CommonException("error.status.status_notice_exist");
        }
        // 校验是否存在执行状态变更配置
        Long organizationId = ConvertUtil.getOrganizationId(projectId);
        List<ExecutionCaseStatusChangeSettingVO> executionCaseStatusChangeSettingVOS =
                testServiceClientOperator.list(projectId, organizationId, issueTypeId, new ArrayList<>(Arrays.asList(currentStatusId)));
        if (!CollectionUtils.isEmpty(executionCaseStatusChangeSettingVOS)) {
            throw new CommonException("error.execution.status_change_exist");
        }

        // 校验是否存在分支合并配置
        List<StatusBranchMergeSettingVO> statusBranchMergeSettingVOS = statusBranchMergeSettingService.listByOptions(projectId, organizationId, issueTypeId, new ArrayList<>(Arrays.asList(currentStatusId)));
        if (!CollectionUtils.isEmpty(statusBranchMergeSettingVOS)) {
            StatusBranchMergeSettingVO statusBranchMergeSettingVO = statusBranchMergeSettingVOS.get(0);
            if (Boolean.TRUE.equals(statusBranchMergeSettingVO.getAutoTransform())) {
                throw new CommonException("error.status.branch_merge_setting_exist");
            }
        }
        
        // 校验是否存在关联问题联动设置
        List<LinkIssueStatusLinkageVO> linkIssueStatusLinkageVOS = linkIssueStatusLinkageService.listByIssueTypeAndStatusId(projectId, organizationId, issueTypeId, currentStatusId);
        if (!CollectionUtils.isEmpty(linkIssueStatusLinkageVOS)) {
            throw new CommonException("error.link.issue.status.linkage.exist");
        }

        return machineNodeDTO;
    }

    @Override
    public Page<StatusSettingVO> statusTransformSettingList(Long projectId, Long issueTypeId, PageRequest pageRequest,
                                                            String param,String applyType,String schemeCode) {
        if (Boolean.FALSE.equals(EnumUtil.contain(SchemeApplyType.class, applyType))) {
            throw new CommonException(ERROR_APPLYTYPE_ILLEGAL);
        }
        // 获取状态加Id
        Long stateMachineId = queryStateMachineIdAndCheck(projectId, applyType, issueTypeId);
        Long organizationId = ConvertUtil.getOrganizationId(projectId);
        Page<StatusSettingVO> page = PageHelper.doPageAndSort(pageRequest, () -> statusMapper.listStatusTransferByStateMachineId(organizationId, stateMachineId,param));
        List<StatusSettingVO> list = page.getContent();
        if (CollectionUtils.isEmpty(list)) {
            return new Page<>();
        }
        List<Long> statusIds = list.stream().map(StatusSettingVO::getId).collect(Collectors.toList());
        List<StatusTransferSettingVO> transferSettingVOS = statusTransferSettingService.listByStatusIds(projectId, issueTypeId, statusIds);
        List<StatusFieldSettingVO> statusFieldSettingVOS = statusFieldSettingService.listByStatusIds(projectId, issueTypeId, statusIds);
        List<StatusNoticeSettingVO> statusNoticeSettingVOS = statusNoticeSettingService.list(projectId, issueTypeId, statusIds, schemeCode);
        List<StatusLinkageVO> linkageVOS = statusLinkageService.listByStatusIds(projectId, issueTypeId, statusIds, applyType);
        Map<Long, List<StatusBranchMergeSettingVO>> statusBranchMergeSettingMap =
                statusBranchMergeSettingService.listByOptions(projectId, organizationId, issueTypeId, statusIds)
                        .stream()
                        .collect(Collectors.groupingBy(StatusBranchMergeSettingVO::getStatusId));
        Map<Long, List<StatusTransferSettingVO>> transferSettingMap = new HashMap<>();
        Map<Long, List<StatusFieldSettingVO>> statusFieldSettingMap = new HashMap<>();
        Map<Long, List<StatusNoticeSettingVO>> statusNoticSettingMap = statusNoticeSettingVOS.stream()
                .collect(Collectors.groupingBy(StatusNoticeSettingVO::getStatusId));
        Map<Long, List<StatusLinkageVO>> statusLinkageMap = linkageVOS.stream()
                .collect(Collectors.groupingBy(StatusLinkageVO::getStatusId));
        Map<Long, List<LinkIssueStatusLinkageVO>> linkIssueMap = linkIssueStatusLinkageService.listByIssueTypeAndStatusIds(projectId, organizationId, issueTypeId, statusIds);
        if (!CollectionUtils.isEmpty(transferSettingVOS)) {
            transferSettingMap.putAll(transferSettingVOS.stream().collect(Collectors.groupingBy(StatusTransferSettingVO::getStatusId)));
        }
        if (!CollectionUtils.isEmpty(statusFieldSettingVOS)) {
            statusFieldSettingMap.putAll(statusFieldSettingVOS.stream().collect(Collectors.groupingBy(StatusFieldSettingVO::getStatusId)));
        }
        Map<Long, ExecutionCaseStatusChangeSettingVO> statusChangeSettingVOMap = getStatusChangeSettingVOMap(projectId, organizationId, issueTypeId, statusIds);
        for (StatusSettingVO statusSettingVO : list) {
            statusSettingVO.setStatusTransferSettingVOS(transferSettingMap.get(statusSettingVO.getId()));
            statusSettingVO.setStatusFieldSettingVOS(statusFieldSettingMap.get(statusSettingVO.getId()));
            statusSettingVO.setStatusNoticeSettingVOS(statusNoticSettingMap.get(statusSettingVO.getId()));
            statusSettingVO.setStatusLinkageVOS(statusLinkageMap.get(statusSettingVO.getId()));
            statusSettingVO.setExecutionCaseStatusChangeSettingVO(statusChangeSettingVOMap.get(statusSettingVO.getId()));
            List<StatusBranchMergeSettingVO> statusBranchMergeSettingList = statusBranchMergeSettingMap.get(statusSettingVO.getId());
            if (!ObjectUtils.isEmpty(statusBranchMergeSettingList)) {
                statusSettingVO.setStatusBranchMergeSettingVO(statusBranchMergeSettingList.get(0));
            }
            statusSettingVO.setLinkIssueStatusLinkageVOS(linkIssueMap.getOrDefault(statusSettingVO.getId(), new ArrayList<>()));
        }
        AgilePluginService agilePluginService = SpringBeanUtil.getExpandBean(AgilePluginService.class);
        if (agilePluginService != null) {
            agilePluginService.listStatusLinkageByStatusIds(projectId, issueTypeId, statusIds, applyType, list);
        }
        page.setContent(list);
        return page;
    }

    private Map<Long, ExecutionCaseStatusChangeSettingVO> getStatusChangeSettingVOMap(Long projectId, Long organizationId, Long issueTypeId, List<Long> statusIds) {
        Map<Long, ExecutionCaseStatusChangeSettingVO> statusChangeSettingVOMap = new HashMap<>();
        List<ExecutionCaseStatusChangeSettingVO> list = testServiceClientOperator.list(projectId, organizationId, issueTypeId, statusIds);
        if (!CollectionUtils.isEmpty(list)) {
            statusChangeSettingVOMap.putAll(list.stream().collect(Collectors.toMap(ExecutionCaseStatusChangeSettingVO::getAgileStatusId, Function.identity())));
        }
        return statusChangeSettingVOMap;
    }

    @Override
    public void handlerDeleteStatusByProject(Long projectId, List<String> applyTypes, Long statusId, List<DeleteStatusTransferVO> statusTransferVOS) {
        Long organizationId = ConvertUtil.getOrganizationId(projectId);
        List<StatusMachineSchemeConfigVO> statusMachineSchemeConfigVOS = statusMachineSchemeConfigMapper.queryStatusMachineMapByAppleTypes(organizationId, projectId, applyTypes);
        Map<Long, DeleteStatusTransferVO> map = statusTransferVOS.stream().collect(Collectors.toMap(DeleteStatusTransferVO::getIssueTypeId, Function.identity()));
        List<Long> filterIssueType = statusService.filterIssueType(projectId, applyTypes);
        for (StatusMachineSchemeConfigVO schemeConfigVO : statusMachineSchemeConfigVOS) {
            Boolean isAgileFeature = Objects.equals(SchemeApplyType.AGILE, schemeConfigVO.getApplyType()) && filterIssueType.contains(schemeConfigVO.getIssueTypeId());
            if (isAgileFeature || Objects.equals(0L, schemeConfigVO.getIssueTypeId())) {
                continue;
            }
            // 查询状态的node
            StatusMachineNodeDTO statusMachineNodeDTO = new StatusMachineNodeDTO();
            statusMachineNodeDTO.setStatusId(statusId);
            statusMachineNodeDTO.setStateMachineId(schemeConfigVO.getStateMachineId());
            statusMachineNodeDTO.setOrganizationId(organizationId);
            StatusMachineNodeDTO machineNodeDTO = statusMachineNodeMapper.selectOne(statusMachineNodeDTO);
            if (!ObjectUtils.isEmpty(machineNodeDTO)) {
                Long tansferStatusId = handlerTransferStatus(machineNodeDTO, map, schemeConfigVO);
                // 删除node
                deleteNode(projectId, schemeConfigVO.getIssueTypeId(), schemeConfigVO.getApplyType(), machineNodeDTO.getId(), tansferStatusId);
            }

        }
    }

    @Override
    public void checkDeleteStatusByProject(Long projectId, List<String> applyTypes, Long statusId) {
        Long organizationId = ConvertUtil.getOrganizationId(projectId);
        List<StatusMachineSchemeConfigVO> stateMachineSchemeConfigVOS = statusMachineSchemeConfigMapper.queryStatusMachineMapByAppleTypes(organizationId, projectId, applyTypes);
        List<Long> filterIssueType = statusService.filterIssueType(projectId, applyTypes);
        for (StatusMachineSchemeConfigVO schemeConfigVO : stateMachineSchemeConfigVOS) {
            Boolean isAgileFeature = Objects.equals(SchemeApplyType.AGILE, schemeConfigVO.getApplyType()) && filterIssueType.contains(schemeConfigVO.getIssueTypeId());
            if (isAgileFeature || Objects.equals(0L, schemeConfigVO.getIssueTypeId())) {
                continue;
            }
            // 查询状态的node
            StatusMachineNodeDTO statusMachineNodeDTO = new StatusMachineNodeDTO();
            statusMachineNodeDTO.setStatusId(statusId);
            statusMachineNodeDTO.setStateMachineId(schemeConfigVO.getStateMachineId());
            statusMachineNodeDTO.setOrganizationId(organizationId);
            StatusMachineNodeDTO machineNodeDTO = statusMachineNodeMapper.selectOne(statusMachineNodeDTO);
            if (!ObjectUtils.isEmpty(machineNodeDTO)) {
                // 检查是否能删除node
                checkDeleteNode(projectId, schemeConfigVO.getIssueTypeId(), schemeConfigVO.getApplyType(), machineNodeDTO.getId());
            }
        }
    }

    @Override
    public void updateNodeObjectVersionNumber(Long project, Long issueType, Long statusId, Long objectVersionNumber, String applyType) {
        Long stateMachineId = queryStateMachineId(project, applyType, issueType);
        // 查询状态的node
        StatusMachineNodeDTO statusMachineNodeDTO = new StatusMachineNodeDTO();
        statusMachineNodeDTO.setStatusId(statusId);
        statusMachineNodeDTO.setStateMachineId(stateMachineId);
        statusMachineNodeDTO.setOrganizationId(ConvertUtil.getOrganizationId(project));
        StatusMachineNodeDTO machineNodeDTO = statusMachineNodeMapper.selectOne(statusMachineNodeDTO);
        if (ObjectUtils.isEmpty(machineNodeDTO)) {
            throw new CommonException("error.node.null");
        }
        machineNodeDTO.setObjectVersionNumber(objectVersionNumber);
        if (statusMachineNodeMapper.updateOptional(machineNodeDTO, "objectVersionNumber") != 1) {
            throw new CommonException("error.update.node");
        }
    }

    @Override
    public void initIssueTypeStatusMachine(Long projectId, String applyType) {
        Long organizationId = ConvertUtil.getOrganizationId(projectId);
        ProjectConfigDTO projectConfigDTO = projectConfigMapper.queryBySchemeTypeAndApplyType(projectId, SchemeType.ISSUE_TYPE, applyType);
        ProjectConfigDTO configDTO = projectConfigMapper.queryBySchemeTypeAndApplyType(projectId, SchemeType.STATE_MACHINE, applyType);
        if (ObjectUtils.isEmpty(projectConfigDTO)) {
            return;
        }
        if (ObjectUtils.isEmpty(configDTO)) {
            return;
        }
        Long stateMachineSchemeId = configDTO.getSchemeId();
        Long schemeId = projectConfigDTO.getSchemeId();
        IssueTypeSchemeConfigDTO issueTypeSchemeConfigDTO = new IssueTypeSchemeConfigDTO();
        issueTypeSchemeConfigDTO.setSchemeId(schemeId);
        issueTypeSchemeConfigDTO.setOrganizationId(organizationId);
        List<IssueTypeSchemeConfigDTO> issueTypeSchemeConfigDTOS = issueTypeSchemeConfigMapper.select(issueTypeSchemeConfigDTO);
        if (org.springframework.util.CollectionUtils.isEmpty(issueTypeSchemeConfigDTOS)) {
            return;
        }
        List<Long> issueTypeIds = issueTypeSchemeConfigDTOS.stream().map(IssueTypeSchemeConfigDTO::getIssueTypeId).collect(Collectors.toList());
        for (Long issueTypeId : issueTypeIds) {
            stateMachineSchemeConfigService.queryStatusMachineBySchemeIdAndIssueType(organizationId, stateMachineSchemeId, issueTypeId);
        }
    }

    private Long handlerTransferStatus(StatusMachineNodeDTO machineNodeDTO, Map<Long, DeleteStatusTransferVO> map, StatusMachineSchemeConfigVO schemeConfigVO){
        // 判断是不是默认状态
        if (!Objects.equals(schemeConfigVO.getIssueTypeId(), 0L) && NodeType.INIT.equals(machineNodeDTO.getType())) {
            throw new CommonException("error.node.is.default.status");
        }
        Long transferStatusId = null;
        DeleteStatusTransferVO deleteStatusTransferVO = map.get(schemeConfigVO.getIssueTypeId());
        if (!ObjectUtils.isEmpty(deleteStatusTransferVO)) {
            transferStatusId = deleteStatusTransferVO.getStatusId();
        }
        return transferStatusId;
    }

    private Boolean checkIssueUse(Long projectId,Long issueTypeId,Long statusId) {
        if (issueTypeId == 0) {
            return false;
        }
        IssueDTO issueDTO = new IssueDTO();
        issueDTO.setProjectId(projectId);
        issueDTO.setStatusId(statusId);
        issueDTO.setIssueTypeId(issueTypeId);
        List<IssueDTO> select = issueMapper.select(issueDTO);
        return !CollectionUtils.isEmpty(select);
    }

    private Long queryStateMachineIdAndCheck(Long projectId, String applyType, Long issueTypeId) {
        applyType = getApplyType(projectId, issueTypeId);
        if (Boolean.FALSE.equals(EnumUtil.contain(SchemeApplyType.class, applyType))) {
            throw new CommonException(ERROR_APPLYTYPE_ILLEGAL);
        }
        Long organizationId = projectUtil.getOrganizationId(projectId);
        Long issueTypeSchemeId = projectConfigMapper.queryBySchemeTypeAndApplyType(projectId, SchemeType.ISSUE_TYPE, applyType).getSchemeId();
        Long stateMachineSchemeId = projectConfigMapper.queryBySchemeTypeAndApplyType(projectId, SchemeType.STATE_MACHINE, applyType).getSchemeId();
        if (ObjectUtils.isEmpty(issueTypeSchemeId)) {
            throw new CommonException("error.queryStateMachineId.issueTypeSchemeId.null");
        }
        if (ObjectUtils.isEmpty(stateMachineSchemeId)) {
            throw new CommonException("error.queryStateMachineId.getStateMachineSchemeId.null");
        }
        return stateMachineSchemeConfigService.queryStatusMachineBySchemeIdAndIssueType(organizationId,stateMachineSchemeId,issueTypeId);
    }

    @Override
    public Long queryStateMachineId(Long projectId, String applyType, Long issueTypeId) {
        if (Boolean.FALSE.equals(EnumUtil.contain(SchemeApplyType.class, applyType))) {
            throw new CommonException(ERROR_APPLYTYPE_ILLEGAL);
        }
        Long organizationId = projectUtil.getOrganizationId(projectId);
        Long issueTypeSchemeId = projectConfigMapper.queryBySchemeTypeAndApplyType(projectId, SchemeType.ISSUE_TYPE, applyType).getSchemeId();
        Long stateMachineSchemeId = projectConfigMapper.queryBySchemeTypeAndApplyType(projectId, SchemeType.STATE_MACHINE, applyType).getSchemeId();
        if (issueTypeSchemeId == null) {
            throw new CommonException("error.queryStateMachineId.issueTypeSchemeId.null");
        }

        if (stateMachineSchemeId == null) {
            throw new CommonException("error.queryStateMachineId.getStateMachineSchemeId.null");
        }
        return stateMachineSchemeConfigService.queryStateMachineIdBySchemeIdAndIssueTypeId(false, organizationId, stateMachineSchemeId, issueTypeId);
    }

    @Override
    public StatusVO createStatusForAgile(Long projectId, String applyType, StatusVO statusVO) {
        Long organizationId = projectUtil.getOrganizationId(projectId);
        statusVO.setOrganizationId(organizationId);
        Map<String, Object> result = checkCreateStatusForAgile(projectId, applyType);
        if (Boolean.TRUE.equals(result.get(FLAG))) {
            Long stateMachineId = (Long) result.get(STATEMACHINEID);
            statusVO = statusService.createStatusForAgile(organizationId, stateMachineId, statusVO);
        } else {
            return null;
        }
        return statusVO;
    }

    @Override
    public Map<String, Object> checkCreateStatusForAgile(Long projectId, String applyType) {
        Map<String, Object> result = new HashMap<>(3);
        result.put(FLAG, true);
        Long organizationId = projectUtil.getOrganizationId(projectId);
        Long stateMachineSchemeId = projectConfigMapper.queryBySchemeTypeAndApplyType(projectId, SchemeType.STATE_MACHINE, applyType).getSchemeId();
        //校验状态机方案是否只关联一个项目
        ProjectConfigDTO select = new ProjectConfigDTO();
        select.setSchemeId(stateMachineSchemeId);
        select.setSchemeType(SchemeType.STATE_MACHINE);
        select.setApplyType(SchemeApplyType.AGILE);
        if (projectConfigMapper.select(select).size() > 1) {
            result.put(FLAG, false);
            result.put(MESSAGE, "error.stateMachineScheme.multiScheme");
            return result;
        }
        //校验状态机方案是否只有一个状态机
        if (stateMachineSchemeConfigService.queryBySchemeId(false, organizationId, stateMachineSchemeId).size() > 1) {
            result.put(FLAG, false);
            result.put(MESSAGE, "error.stateMachineScheme.multiStateMachine");
            return result;
        }
        Long stateMachineId = stateMachineSchemeConfigService.selectDefault(false, organizationId, stateMachineSchemeId).getStateMachineId();
        if (stateMachineId == null) {
            result.put(FLAG, false);
            result.put(MESSAGE, "error.stateMachineScheme.defaultStateMachineId.notNull");
            return result;
        }
        //校验这个状态机是否只关联一个方案
        List<Long> schemeIds = stateMachineSchemeConfigService.querySchemeIdsByStateMachineId(false, organizationId, stateMachineId);
        if (schemeIds.size() > 1) {
            result.put(FLAG, false);
            result.put(MESSAGE, "error.stateMachineScheme.stateMachineInMoreThanOneScheme");
            return result;
        }
        result.put(STATEMACHINEID, stateMachineId);
        return result;
    }

    @Override
    public void removeStatusForAgile(Long projectId, Long statusId, String applyType) {
        Map<String, Object> result = checkCreateStatusForAgile(projectId, applyType);
        Boolean flag = (Boolean) result.get(FLAG);
        if (Boolean.TRUE.equals(flag)) {
            Long stateMachineId = (Long) result.get(STATEMACHINEID);
            Long organizationId = projectUtil.getOrganizationId(projectId);
            Long initStatusId = instanceService.queryInitStatusId(organizationId, stateMachineId);
            if (statusId.equals(initStatusId)) {
                throw new CommonException("error.initStatus.illegal");
            }
            if (Boolean.FALSE.equals(checkStatusInBoardColumn(projectId, statusId))) {
                throw new CommonException("error.status.inBoardColumn");
            }
            statusService.removeStatusForAgile(organizationId, stateMachineId, statusId);
            IssueStatusDTO issueStatusDTO = new IssueStatusDTO();
            issueStatusDTO.setProjectId(projectId);
            issueStatusDTO.setStatusId(statusId);
            issueStatusMapper.delete(issueStatusDTO);
        } else {
            throw new RemoveStatusException((String) result.get(MESSAGE));
        }
    }

    private Boolean checkStatusInBoardColumn(Long projectId, Long statusId) {
        ColumnStatusRelDTO columnStatusRelDTO = new ColumnStatusRelDTO();
        columnStatusRelDTO.setProjectId(projectId);
        columnStatusRelDTO.setStatusId(statusId);
        List<ColumnStatusRelDTO> columnStatusRelDTOList = columnStatusRelMapper.select(columnStatusRelDTO);
        return  CollectionUtils.isEmpty(columnStatusRelDTOList);
    }

    @Override
    public Boolean checkRemoveStatusForAgile(Long projectId, Long statusId, String applyType) {
        Map<String, Object> result = checkCreateStatusForAgile(projectId, applyType);
        if (Boolean.TRUE.equals(result.get(FLAG))) {
            Long stateMachineId = (Long) result.get(STATEMACHINEID);
            Long organizationId = projectUtil.getOrganizationId(projectId);
            Long initStatusId = instanceService.queryInitStatusId(organizationId, stateMachineId);
            if (!statusId.equals(initStatusId)) {
                return checkStatusInBoardColumn(projectId, statusId);
            } else {
                return false;
            }
        } else {
            throw new RemoveStatusException((String) result.get(MESSAGE));
        }
    }

    @Override
    public Map<String, List<Long>> queryProjectIdsMap(Long organizationId, Long stateMachineId) {
        //查询状态机方案中的配置
        List<Long> schemeIds = stateMachineSchemeConfigService.querySchemeIdsByStateMachineId(false, organizationId, stateMachineId);

        if (!schemeIds.isEmpty()) {
            List<ProjectConfigDTO> projectConfigs = projectConfigMapper.queryBySchemeIds(schemeIds, SchemeType.STATE_MACHINE);
            return projectConfigs.stream().collect(Collectors.groupingBy(ProjectConfigDTO::getApplyType, Collectors.mapping(ProjectConfigDTO::getProjectId, Collectors.toList())));
        }
        return Collections.emptyMap();
    }

    @Override
    public Long queryWorkFlowFirstStatus(Long projectId, String applyType, Long issueTypeId, Long organizationId) {
        Long statusMachineId = projectConfigService.queryStateMachineId(projectId, applyType, issueTypeId);
        if (statusMachineId == null) {
            throw new CommonException(ERROR_ISSUE_STATE_MACHINE_NOT_FOUND);
        }
        Long initStatusId = instanceService.queryInitStatusId(organizationId, statusMachineId);
        if (initStatusId == null) {
            throw new CommonException(ERROR_ISSUE_STATUS_NOT_FOUND);
        }
        return initStatusId;
    }
}
