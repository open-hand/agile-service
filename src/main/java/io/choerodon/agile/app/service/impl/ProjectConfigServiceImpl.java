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
import io.choerodon.agile.infra.feign.vo.ProjectCategoryDTO;
import io.choerodon.agile.infra.mapper.*;
import io.choerodon.agile.infra.utils.*;
import io.choerodon.core.domain.Page;
import io.choerodon.core.exception.CommonException;
import io.choerodon.core.oauth.CustomUserDetails;
import io.choerodon.core.oauth.DetailsHelper;
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
public class ProjectConfigServiceImpl implements ProjectConfigService {

    private static final String AGILE_SERVICE = "agile-service";
    private static final String FLAG = "flag";
    private static final String MESSAGE = "message";
    private static final String STATEMACHINEID = "stateMachineId";
    private static final String ERROR_ISSUE_STATE_MACHINE_NOT_FOUND = "error.issueStateMachine.notFound";
    private static final String ERROR_ISSUE_STATUS_NOT_FOUND = "error.createIssue.issueStatusNotFound";
    private static final String ERROR_APPLYTYPE_ILLEGAL = "error.applyType.illegal";
    private static final String ERROR_STATEMACHINESCHEMEID_NULL = "error.stateMachineSchemeId.null";

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

    @Override
    public ProjectConfigDTO create(Long projectId, Long schemeId, String schemeType, String applyType) {
        if (!EnumUtil.contain(SchemeType.class, schemeType)) {
            throw new CommonException("error.schemeType.illegal");
        }
        if (!EnumUtil.contain(SchemeApplyType.class, applyType)) {
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

//        //若是关联状态机方案，设置状态机方案、状态机为活跃
//        if (schemeType.equals(SchemeType.STATE_MACHINE)) {
//            stateMachineSchemeService.activeSchemeWithRefProjectConfig(schemeId);
//        }
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
                StateMachineSchemeVO stateMachineSchemeVO = stateMachineSchemeService.querySchemeWithConfigById(false, organizationId, projectConfig.getSchemeId());
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
        if (!EnumUtil.contain(SchemeApplyType.class, applyType)) {
            throw new CommonException(ERROR_APPLYTYPE_ILLEGAL);
        }
        Long organizationId = projectUtil.getOrganizationId(projectId);
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
        if (!EnumUtil.contain(SchemeApplyType.class, applyType)) {
            throw new CommonException(ERROR_APPLYTYPE_ILLEGAL);
        }
        Long organizationId = projectUtil.getOrganizationId(projectId);
        Long issueTypeSchemeId = projectConfigMapper.queryBySchemeTypeAndApplyType(projectId, SchemeType.ISSUE_TYPE, applyType).getSchemeId();
        Long stateMachineSchemeId = projectConfigMapper.queryBySchemeTypeAndApplyType(projectId, SchemeType.STATE_MACHINE, applyType).getSchemeId();
        if (issueTypeSchemeId == null) {
            throw new CommonException("error.issueTypeSchemeId.null");
        }
        if (stateMachineSchemeId == null) {
            throw new CommonException(ERROR_STATEMACHINESCHEMEID_NULL);
        }
        //根据方案配置表获取 问题类型
        List<IssueTypeDTO> issueTypes = issueTypeMapper.queryBySchemeId(organizationId, projectId, issueTypeSchemeId, onlyEnabled);
        //根据方案配置表获取 状态机与问题类型的对应关系
        List<StatusMachineSchemeConfigVO> configs = stateMachineSchemeConfigService.queryBySchemeId(false, organizationId, stateMachineSchemeId);
        Map<Long, Long> map = configs.stream().collect(Collectors.toMap(StatusMachineSchemeConfigVO::getIssueTypeId, StatusMachineSchemeConfigVO::getStateMachineId));
        Long defaultStateMachineId = stateMachineSchemeConfigService.selectDefault(false, organizationId, stateMachineSchemeId).getStateMachineId();
        List<IssueTypeWithStateMachineIdVO> issueTypeWithStateMachineIds = modelMapper.map(issueTypes, new TypeToken<List<IssueTypeWithStateMachineIdVO>>() {
        }.getType());
        issueTypeWithStateMachineIds.forEach(x -> {
            Long stateMachineId = map.get(x.getId());
            if (stateMachineId != null) {
                x.setStateMachineId(stateMachineId);
            } else {
                x.setStateMachineId(defaultStateMachineId);
            }
        });
        return issueTypeWithStateMachineIds;
    }

    @Override
    public List<StatusVO> queryStatusByIssueTypeId(Long projectId, Long issueTypeId, String applyType) {
        Long organizationId = projectUtil.getOrganizationId(projectId);
        Long stateMachineSchemeId = projectConfigMapper.queryBySchemeTypeAndApplyType(projectId, SchemeType.STATE_MACHINE, applyType).getSchemeId();
        if (stateMachineSchemeId == null) {
            throw new CommonException(ERROR_STATEMACHINESCHEMEID_NULL);
        }
        //获取状态机
        Long stateMachineId = stateMachineSchemeConfigService.queryStateMachineIdBySchemeIdAndIssueTypeId(false, organizationId, stateMachineSchemeId, issueTypeId);
        return statusService.queryByStateMachineIds(organizationId, Collections.singletonList(stateMachineId));
    }

    @Override
    public List<StatusVO> queryStatusByProjectId(Long projectId, String applyType) {
        ProjectVO projectVO = baseFeignClient.queryProject(projectId).getBody();
        Long organizationId = projectVO.getOrganizationId();
        Long stateMachineSchemeId = projectConfigMapper.queryBySchemeTypeAndApplyType(projectId, SchemeType.STATE_MACHINE, applyType).getSchemeId();
        if (stateMachineSchemeId == null) {
            throw new CommonException(ERROR_STATEMACHINESCHEMEID_NULL);
        }
        //获取状态机ids
        List<Long> issueTypeIds = new ArrayList<>();
        if(Objects.equals(applyType,SchemeApplyType.AGILE) || Objects.equals(applyType,SchemeApplyType.PROGRAM)){
            issueTypeIds.add(0L);
            List<String> categoryCodes = projectVO.getCategories().stream().map(ProjectCategoryDTO::getCode).collect(Collectors.toList());
            if (categoryCodes.contains(ProjectCategory.MODULE_AGILE)) {
                IssueTypeDTO issueType = new IssueTypeDTO();
                issueType.setOrganizationId(organizationId);
                issueType.setTypeCode("feature");
                List<IssueTypeDTO> issueTypeDTOS = issueTypeMapper.select(issueType);
                if (!CollectionUtils.isEmpty(issueTypeDTOS)) {
                    issueTypeDTOS.forEach(x -> issueTypeIds.add(x.getId()));
                }
            }
        }
        List<Long> stateMachineIds = stateMachineSchemeConfigService.queryBySchemeId(false, organizationId, stateMachineSchemeId)
                .stream()
                .filter(v -> !issueTypeIds.contains(v.getIssueTypeId()))
                .map(StatusMachineSchemeConfigVO::getStateMachineId).collect(Collectors.toList());
        return statusService.queryByStateMachineIds(organizationId, stateMachineIds);
    }

    @Override
    public List<TransformVO> queryTransformsByProjectId(Long projectId, Long currentStatusId, Long issueId, Long issueTypeId, String applyType) {
        if (!EnumUtil.contain(SchemeApplyType.class, applyType)) {
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
        if (!EnumUtil.contain(SchemeApplyType.class, applyType)) {
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
            boolean isSkip = "issue_epic".equals(v.getTypeCode()) || (SchemeApplyType.AGILE.equals(applyType) && "feature".equals(v.getTypeCode()));
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
//        Long defaultStateMachineId = idMap.get(0L);
//        resultMap.put(0L, statusMap.get(defaultStateMachineId));

        Set<Long> boardStatus = new HashSet<>();
        if (!ObjectUtils.isEmpty(boardId)) {
            // 查询出面板上有的状态
            boardStatus.addAll(boardColumnMapper.queryStatusByBoardId(projectId,boardId));
        }
        //匹配状态机的问题类型映射
        for (IssueTypeDTO issueType : issueTypes) {
            boolean isSkip = "issue_epic".equals(issueType.getTypeCode()) || (SchemeApplyType.AGILE.equals(applyType) && "feature".equals(issueType.getTypeCode()));
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
                statusTransferMap.entrySet().stream().filter(entry -> entry.getKey() != 0L && boardStatus.contains(entry.getKey())).forEach(entry -> {
                    transferMap.put(entry.getKey(),entry.getValue().stream().filter(v ->  canTransferStatus.contains(v.getEndStatusId())).map(v -> {
                        StatusVO statusVO = sMap.get(v.getEndStatusId());
                        if (statusVO != null) {
                            v.setStatusType(statusVO.getType());
                        }
                        return v;
                    }).collect(Collectors.toList()));
                });
                resultMap.put(issueType.getId(),transferMap);
            }
        }
        return resultMap;
    }

    @Override
    public List<StatusAndTransformVO> statusTransformList(Long projectId, Long issueTypeId, String applyType) {
        if (!EnumUtil.contain(SchemeApplyType.class, applyType)) {
            throw new CommonException(ERROR_APPLYTYPE_ILLEGAL);
        }
        // 获取状态加Id
        Long stateMachineId = queryStateMachineIdAndCheck(projectId, applyType, issueTypeId);
        Long organizationId = ConvertUtil.getOrganizationId(projectId);
        // 查询当前状态机有哪些状态
        List<StatusAndTransformVO> statusVOS = statusService.queryStatusByStateMachineId(organizationId, stateMachineId);
        if (CollectionUtils.isEmpty(statusVOS)) {
            return new ArrayList<>();
        }
        List<Long> issueUseStatusId = issueMapper.selectStatusIdByIssueType(projectId,issueTypeId);
        //状态机id->状态id->转换列表
        Map<Long, Map<Long, List<TransformVO>>> statusMap = transformService.queryStatusTransformsMap(organizationId, Arrays.asList(stateMachineId));
        Map<Long, List<TransformVO>> listMap = statusMap.get(stateMachineId);
        int index = 0;
        for (StatusAndTransformVO item : statusVOS) {
            if (Boolean.TRUE.equals(item.getDefaultStatus())) {
                index = statusVOS.indexOf(item);
            }
            List<TransformVO> transformVOS = listMap.get(item.getId());
            if (!CollectionUtils.isEmpty(transformVOS)) {
                item.setCanTransformStatus(transformVOS.stream().map(TransformVO::getEndStatusId).collect(Collectors.toSet()));
            }

            item.setHasIssue(issueUseStatusId.contains(item.getId()));
        }
        if (index > 0) {
            Collections.swap(statusVOS, 0, index);
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
        if (statusCheckVO.getStatusExist()) {
            StatusDTO statusInDb = statusMapper.queryById(organizationId, statusCheckVO.getId());
            status = modelMapper.map(statusInDb, StatusVO.class);
        }else {
            status = statusService.create(organizationId, statusVO);
        }
        // 关联状态机
        if (!CollectionUtils.isEmpty(issueTypeIds)) {
            for (Long issueTypeId:issueTypeIds) {
                linkStatus(projectId,issueTypeId,applyType,status.getId(),statusVO.getDefaultStatus(), statusVO.getTransferAll());
            }
        }
        return status;
    }

    @Override
    public StatusMachineNodeVO linkStatus(Long projectId, Long issueTypeId, String applyType, Long statusId, Boolean defaultStatus, Boolean transferAll) {
        Long organizationId = ConvertUtil.getOrganizationId(projectId);
        IssueStatusDTO issueStatusDTO = issueStatusMapper.selectByStatusId(projectId, statusId);
        StatusVO statusVO = statusService.queryStatusById(organizationId, statusId);
        if (ObjectUtils.isEmpty(issueStatusDTO)) {
            issueStatusDTO = new IssueStatusDTO();
            issueStatusDTO.setProjectId(projectId);
            issueStatusDTO.setStatusId(statusId);
            issueStatusDTO.setCompleted(false);
            issueStatusDTO.setName(statusVO.getName());
            issueStatusDTO.setCategoryCode(statusVO.getType());
            issueStatusDTO.setEnable(false);
            issueStatusService.insertIssueStatus(issueStatusDTO);
        }
        Long stateMachineId = queryStateMachineIdAndCheck(projectId, applyType, issueTypeId);
        StatusMachineNodeDTO stateMachineNode = new StatusMachineNodeDTO();
        stateMachineNode.setStatusId(statusId);
        stateMachineNode.setOrganizationId(organizationId);
        stateMachineNode.setStateMachineId(stateMachineId);
        // 校验是否已存在关联的状态
        List<StatusMachineNodeDTO> select = statusMachineNodeMapper.select(stateMachineNode);
        if (CollectionUtils.isEmpty(select)) {
            List<StatusMachineNodeVO> statusMachineNodeVOS = stateMachineNodeService.queryByStateMachineId(organizationId, stateMachineId, false);
            stateMachineNode.setType(NodeType.CUSTOM);
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

    private void transformAll(List<StatusMachineNodeVO> statusMachineNodeVOS, Long organizationId, Long statusId, Long stateMachineId, Long nodeId) {
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
    @Transactional(rollbackFor = Exception.class)
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
        // 清除状态机实例
        instanceCache.cleanStateMachine(stateMachineId);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
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
        String applyType = Objects.equals(ConvertUtil.queryProject(projectId).getCategory(), ProjectCategory.PROGRAM) ? "program" : "agile";
        Long stateMachineSchemeId = projectConfigMapper.queryBySchemeTypeAndApplyType(projectId, SchemeType.STATE_MACHINE, applyType).getSchemeId();
        if (stateMachineSchemeId == null) {
            throw new CommonException(ERROR_STATEMACHINESCHEMEID_NULL);
        }
        List<Long> issueTypeIds = projectConfigMapper.getExistStatusTypeIds(organizationId,statusId,stateMachineSchemeId);
        if(CollectionUtils.isEmpty(issueTypeIds)){
            return new ArrayList<>();
        }
        IssueTypeSearchVO issueTypeSearchVO = new IssueTypeSearchVO();
        issueTypeSearchVO.setIssueTypeIds(issueTypeIds);
        return issueTypeMapper.selectByOptions(organizationId, projectId, issueTypeSearchVO);
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
        IssueTypeDTO issueTypeDTO = issueTypeMapper.selectByPrimaryKey(issueTypeId);
        List<StatusLinkageDTO> linkExistList = statusLinkageMapper.selectByCondition(Condition.builder(StatusLinkageDTO.class)
                .andWhere(Sqls.custom().andEqualTo("projectId", projectId))
                .andWhere(Sqls.custom().andEqualTo("statusId", currentStatusId).andEqualTo("issueTypeId", issueTypeId))
                .orWhere(Sqls.custom().andEqualTo("parentIssueStatusSetting", currentStatusId).andEqualTo("parentIssueTypeCode", issueTypeDTO.getTypeCode())).build());
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
        Map<Long, List<StatusTransferSettingVO>> transferSettingMap = new HashMap<>();
        Map<Long, List<StatusFieldSettingVO>> statusFieldSettingMap = new HashMap<>();
        Map<Long, List<StatusNoticeSettingVO>> statusNoticSettingMap = statusNoticeSettingVOS.stream()
                .collect(Collectors.groupingBy(StatusNoticeSettingVO::getStatusId));
        Map<Long, List<StatusLinkageVO>> statusLinkageMap = linkageVOS.stream()
                .collect(Collectors.groupingBy(StatusLinkageVO::getStatusId));
        if (!CollectionUtils.isEmpty(transferSettingVOS)) {
            transferSettingMap.putAll(transferSettingVOS.stream().collect(Collectors.groupingBy(StatusTransferSettingVO::getStatusId)));
        }
        if (!CollectionUtils.isEmpty(statusFieldSettingVOS)) {
            statusFieldSettingMap.putAll(statusFieldSettingVOS.stream().collect(Collectors.groupingBy(StatusFieldSettingVO::getStatusId)));
        }

        for (StatusSettingVO statusSettingVO : list) {
            statusSettingVO.setStatusTransferSettingVOS(transferSettingMap.get(statusSettingVO.getId()));
            statusSettingVO.setStatusFieldSettingVOS(statusFieldSettingMap.get(statusSettingVO.getId()));
            statusSettingVO.setStatusNoticeSettingVOS(statusNoticSettingMap.get(statusSettingVO.getId()));
            statusSettingVO.setStatusLinkageVOS(statusLinkageMap.get(statusSettingVO.getId()));
        }
        AgilePluginService agilePluginService = SpringBeanUtil.getExpandBean(AgilePluginService.class);
        if (agilePluginService != null) {
            agilePluginService.listStatusLinkageByStatusIds(projectId, issueTypeId, statusIds, applyType, list);
        }
        page.setContent(list);
        return page;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void handlerDeleteStatusByProject(Long projectId, String applyType, Long statusId, List<DeleteStatusTransferVO> statusTransferVOS) {
        Long stateMachineSchemeId = projectConfigMapper.queryBySchemeTypeAndApplyType(projectId, SchemeType.STATE_MACHINE, applyType).getSchemeId();
        Long organizationId = ConvertUtil.getOrganizationId(projectId);
        List<StatusMachineSchemeConfigVO> statusMachineSchemeConfigVOS = stateMachineSchemeConfigService.queryBySchemeId(false, organizationId, stateMachineSchemeId);
        Map<Long, DeleteStatusTransferVO> map = statusTransferVOS.stream().collect(Collectors.toMap(DeleteStatusTransferVO::getIssueTypeId, Function.identity()));
        List<Long> filterIssueType = statusService.filterIssueType(projectId, applyType);
        for (StatusMachineSchemeConfigVO schemeConfigVO : statusMachineSchemeConfigVOS) {
            if (filterIssueType.contains(schemeConfigVO.getIssueTypeId())) {
                continue;
            }
            // 查询状态的node
            StatusMachineNodeDTO statusMachineNodeDTO = new StatusMachineNodeDTO();
            statusMachineNodeDTO.setStatusId(statusId);
            statusMachineNodeDTO.setStateMachineId(schemeConfigVO.getStateMachineId());
            statusMachineNodeDTO.setOrganizationId(organizationId);
            StatusMachineNodeDTO machineNodeDTO = statusMachineNodeMapper.selectOne(statusMachineNodeDTO);
            if (ObjectUtils.isEmpty(machineNodeDTO)) {
                continue;
            }
            Long tansferStatusId = handlerTransferStatus(machineNodeDTO, map, schemeConfigVO);
            // 删除node
            deleteNode(projectId, schemeConfigVO.getIssueTypeId(), applyType, machineNodeDTO.getId(), tansferStatusId);
        }
    }

    @Override
    public void checkDeleteStatusByProject(Long projectId, String applyType, Long statusId) {
        Long stateMachineSchemeId = projectConfigMapper.queryBySchemeTypeAndApplyType(projectId, SchemeType.STATE_MACHINE, applyType).getSchemeId();
        Long organizationId = ConvertUtil.getOrganizationId(projectId);
        List<StatusMachineSchemeConfigVO> stateMachineSchemeConfigVOS = stateMachineSchemeConfigService.queryBySchemeId(false, organizationId, stateMachineSchemeId);
        List<Long> filterIssueType = statusService.filterIssueType(projectId, applyType);
        for (StatusMachineSchemeConfigVO schemeConfigVO : stateMachineSchemeConfigVOS) {
            if (filterIssueType.contains(schemeConfigVO.getIssueTypeId())) {
                continue;
            }
            // 查询状态的node
            StatusMachineNodeDTO statusMachineNodeDTO = new StatusMachineNodeDTO();
            statusMachineNodeDTO.setStatusId(statusId);
            statusMachineNodeDTO.setStateMachineId(schemeConfigVO.getStateMachineId());
            statusMachineNodeDTO.setOrganizationId(organizationId);
            StatusMachineNodeDTO machineNodeDTO = statusMachineNodeMapper.selectOne(statusMachineNodeDTO);
            if (ObjectUtils.isEmpty(machineNodeDTO)) {
                continue;
            }
            // 检查是否能删除node
            checkDeleteNode(projectId, schemeConfigVO.getIssueTypeId(), applyType, machineNodeDTO.getId());
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

    private void updateIssueStatusByStatusId(Long projectId, Long issueTypeId, String applyType, Long currentStatusId, Long statusId, StatusMachineNodeDTO statusMachineNodeDTO){
        if (ObjectUtils.isEmpty(statusId)) {
            throw new CommonException("error.status.has.using");
        } else {
            // 校验状态机node里面存在当前指定状态
            statusMachineNodeDTO.setStatusId(statusId);
            List<StatusMachineNodeDTO> select = statusMachineNodeMapper.select(statusMachineNodeDTO);
            if(CollectionUtils.isEmpty(select)){
                throw new CommonException("error.status.id.illegal");
            }
            // 将关联的状态修改为指定状态
            Long userId = DetailsHelper.getUserDetails().getUserId();
            issueAccessDataService.updateIssueStatusByIssueTypeId(projectId,applyType,issueTypeId,currentStatusId,statusId,userId);
        }
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
        if (!EnumUtil.contain(SchemeApplyType.class, applyType)) {
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
        if ((Boolean) result.get(FLAG)) {
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
        if (flag) {
            Long stateMachineId = (Long) result.get(STATEMACHINEID);
            Long organizationId = projectUtil.getOrganizationId(projectId);
            Long initStatusId = instanceService.queryInitStatusId(organizationId, stateMachineId);
            if (statusId.equals(initStatusId)) {
                throw new CommonException("error.initStatus.illegal");
            }
            if (!checkStatusInBoardColumn(projectId, statusId)) {
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
        if (columnStatusRelDTOList != null && !columnStatusRelDTOList.isEmpty()) {
            return false;
        } else {
            return true;
        }
    }

    @Override
    public Boolean checkRemoveStatusForAgile(Long projectId, Long statusId, String applyType) {
        Map<String, Object> result = checkCreateStatusForAgile(projectId, applyType);
        Boolean flag = (Boolean) result.get(FLAG);
        if (flag) {
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
