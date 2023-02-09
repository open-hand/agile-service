package io.choerodon.agile.app.service.impl;

import java.util.*;
import java.util.stream.Collectors;

import com.google.common.reflect.TypeToken;
import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.api.vo.event.ProjectEvent;
import io.choerodon.agile.api.vo.waterfall.PredecessorIssueStatusLinkageVO;
import io.choerodon.agile.app.service.*;
import io.choerodon.agile.infra.cache.InstanceCache;
import io.choerodon.agile.infra.dto.*;
import io.choerodon.agile.infra.enums.*;
import io.choerodon.agile.infra.feign.operator.RemoteIamOperator;
import io.choerodon.agile.infra.feign.vo.OrganizationInfoVO;
import io.choerodon.agile.infra.mapper.*;
import io.choerodon.agile.infra.utils.ConvertUtil;
import io.choerodon.agile.infra.utils.RankUtil;
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
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.Assert;
import org.springframework.util.ObjectUtils;

/**
 * @author zhaotianxin
 * @date 2021-03-22 14:50
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class OrganizationConfigServiceImpl implements OrganizationConfigService {

    @Autowired
    private OrganizationConfigMapper organizationConfigMapper;
    @Autowired
    private StateMachineSchemeService stateMachineSchemeService;
    @Autowired
    private InitService initService;
    @Autowired
    private StatusMachineMapper statusMachineMapper;
    @Autowired
    private RemoteIamOperator remoteIamOperator;
    @Autowired
    private IssueTypeMapper issueTypeMapper;
    @Autowired
    private StatusMachineSchemeConfigMapper statusMachineSchemeConfigMapper;
    @Autowired
    private StateMachineNodeService stateMachineNodeService;
    @Autowired
    private StatusService statusService;
    @Autowired
    private InstanceCache instanceCache;
    @Autowired
    private StateMachineTransformService transformService;
    @Autowired
    private StatusMachineTransformMapper statusMachineTransformMapper;
    @Autowired
    private StatusMachineNodeMapper statusMachineNodeMapper;
    @Autowired
    private StatusLinkageMapper statusLinkageMapper;
    @Autowired
    private StatusTransferSettingMapper statusTransferSettingMapper;
    @Autowired
    private StatusFieldSettingMapper statusFieldSettingMapper;
    @Autowired
    private StatusNoticeSettingMapper statusNoticeSettingMapper;
    @Autowired
    private ModelMapper modelMapper;
    @Autowired
    private ProjectConfigService projectConfigService;
    @Autowired
    private StatusMapper statusMapper;
    @Autowired
    private StatusNoticeSettingService statusNoticeSettingService;
    @Autowired
    private StatusTransferSettingService statusTransferSettingService;
    @Autowired
    private StatusLinkageService statusLinkageService;
    @Autowired
    private StatusFieldSettingService statusFieldSettingService;
    @Autowired
    private ProjectConfigMapper projectConfigMapper;
    @Autowired
    private StateMachineSchemeConfigService stateMachineSchemeConfigService;
    @Autowired
    private IssueTypeSchemeConfigMapper issueTypeSchemeConfigMapper;
    @Autowired
    private StateMachineService stateMachineService;
    @Autowired
    private IssueStatusService issueStatusService;
    @Autowired
    private  BoardMapper boardMapper;
    @Autowired
    private BoardTemplateService boardTemplateService;
    @Autowired
    private IssueTypeService issueTypeService;
    @Autowired
    private StatusTemplateMapper statusTemplateMapper;
    @Autowired
    private StatusBranchMergeSettingService statusBranchMergeSettingService;
    @Autowired(required = false)
    private AgileWaterfallService agileWaterfallService;

    @Override
    public OrganizationConfigDTO initStatusMachineTemplate(Long organizationId, Long issueTypeId) {
        OrganizationConfigDTO configDTO = querySchemeId(organizationId, SchemeType.STATE_MACHINE, SchemeApplyType.AGILE);
        if (ObjectUtils.isEmpty(configDTO)) {
            // 创建状态机方案
            Long schemeId = stateMachineSchemeService.initOrgDefaultStatusMachineScheme(organizationId);
            // 初始化问题类型的状态机
            initStatusMachine(organizationId, issueTypeId, schemeId);
            // 组织和方案建立关联
            OrganizationConfigDTO organizationConfigDTO = new OrganizationConfigDTO();
            organizationConfigDTO.setOrganizationId(organizationId);
            organizationConfigDTO.setSchemeId(schemeId);
            organizationConfigDTO.setSchemeType(SchemeType.STATE_MACHINE);
            organizationConfigDTO.setApplyType(SchemeApplyType.AGILE);
            baseInsert(organizationConfigDTO);
            return organizationConfigDTO;
        } else {
            Long schemeId = configDTO.getSchemeId();
            StatusMachineSchemeConfigDTO statusMachineSchemeConfigDTO = queryStatusMachineSchemeConfig(organizationId, issueTypeId, schemeId);
            if (ObjectUtils.isEmpty(statusMachineSchemeConfigDTO)) {
                // 初始化问题类型的状态机
                initStatusMachine(organizationId, issueTypeId, schemeId);
            }
            return configDTO;
        }
    }

    @Override
    public List<StatusAndTransformVO> listTransform(Long organizationId, Long issueTypeId) {
        Long stateMachineId = queryStatusMachineId(organizationId, issueTypeId);
        // 处理rank值
        stateMachineNodeService.handlerNullRankNode(organizationId, stateMachineId, "");
        // 查询当前状态机有哪些状态
        List<StatusAndTransformVO> statusVOS = statusService.queryStatusByStateMachineId(organizationId, null, stateMachineId);
        if (CollectionUtils.isEmpty(statusVOS)) {
            return new ArrayList<>();
        }
        Map<Long, Map<Long, List<TransformVO>>> statusMap = transformService.queryStatusTransformsMap(organizationId, Arrays.asList(stateMachineId));
        Map<Long, List<TransformVO>> listMap = statusMap.get(stateMachineId);
        for (StatusAndTransformVO item : statusVOS) {
            List<TransformVO> transformVOS = listMap.get(item.getId());
            if (!CollectionUtils.isEmpty(transformVOS)) {
                item.setCanTransformStatus(transformVOS.stream().map(TransformVO::getEndStatusId).collect(Collectors.toSet()));
            }
        }
        return statusVOS;
    }

    @Override
    public List<StateMachineTransformUpdateVO> updateTransform(Long organizationId, Long issueTypeId, List<StateMachineTransformUpdateVO> list) {
        Long stateMachineId = queryStatusMachineId(organizationId, issueTypeId);
        if (CollectionUtils.isEmpty(list)) {
            throw new CommonException("error.transform.null");
        }
        for (StateMachineTransformUpdateVO transformUpdateVO : list) {
            if (ObjectUtils.isEmpty(transformUpdateVO.getEndNodeId()) || ObjectUtils.isEmpty(transformUpdateVO.getStartNodeId())) {
                throw new CommonException("error.node.id.null");
            }
            if (Boolean.TRUE.equals(transformUpdateVO.getSelect())) {
                transformService.createTransform(organizationId, stateMachineId, transformUpdateVO);
            } else {
                transformService.deleteTransformByNodeId(organizationId, stateMachineId, transformUpdateVO.getStartNodeId(), transformUpdateVO.getEndNodeId());
            }
        }
        //清理状态机实例
        instanceCache.cleanStateMachine(stateMachineId);
        return list;
    }

    @Override
    public void deleteNode(Long organizationId, Long issueTypeId, Long nodeId) {
        Long statusMachineId = queryStatusMachineId(organizationId, issueTypeId);
        StatusMachineNodeDTO statusMachineNodeDTO = new StatusMachineNodeDTO();
        statusMachineNodeDTO.setOrganizationId(organizationId);
        statusMachineNodeDTO.setStateMachineId(statusMachineId);
        StatusMachineNodeDTO currentNode = checkStatusLink(organizationId, issueTypeId, nodeId);
        Long currentStatusId = currentNode.getStatusId();
        Assert.notNull(currentStatusId, BaseConstants.ErrorCode.DATA_INVALID);
        // 删除当前node的转换
        statusMachineTransformMapper.deleteByStateMachineIdAndNodeId(organizationId,statusMachineId,nodeId);
        // 删除node
        statusMachineNodeDTO.setStatusId(null);
        statusMachineNodeDTO.setId(nodeId);
        statusMachineNodeMapper.delete(statusMachineNodeDTO);
        // 判断当前状态在其他预定义状态问题类型的状态机模板里面是否存在
        boardTemplateService.deleteBoardTemplateStatus(currentStatusId, organizationId);
    }

    @Override
    public StatusMachineNodeVO linkStatus(Long organizationId, Long issueTypeId, Long statusId, Boolean defaultStatus, Boolean transferAll) {
        StatusVO statusVO = statusService.queryStatusById(organizationId, statusId);
        Long stateMachineId = queryStatusMachineId(organizationId, issueTypeId);
        stateMachineNodeService.handlerNullRankNode(organizationId, stateMachineId, "");
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
                defaultStatus(organizationId, stateMachineId, statusId);
            }
            // 默认可以全部流转到当前状态(设置为)
            if (ObjectUtils.isEmpty(transferAll) || Boolean.TRUE.equals(transferAll)) {
                projectConfigService.transformAll(statusMachineNodeVOS, organizationId, statusId, stateMachineId, stateMachineNode.getId());
            }
            else {
                String nodeName = statusVO.getName() + "转换到" + statusVO.getName();
                StatusMachineTransformDTO statusMachineTransformDTO = new StatusMachineTransformDTO(nodeName, stateMachineId, stateMachineNode.getId(), stateMachineNode.getId(), TransformType.CUSTOM, TransformConditionStrategy.ALL, organizationId);
                statusMachineTransformMapper.batchInsert(Arrays.asList(statusMachineTransformDTO));
            }
        }
        return modelMapper.map(stateMachineNode, StatusMachineNodeVO.class);
    }

    @Override
    public void defaultStatus(Long organizationId, Long stateMachineId, Long statusId) {
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
    }

    public StatusVO createStatus(Long organizationId, List<Long> issueTypeIds, StatusVO statusVO) {
        if (ObjectUtils.isEmpty(statusVO.getName()) && ObjectUtils.isEmpty(statusVO.getType())) {
            throw new CommonException("error.status.name.or.type.null");
        }
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
                linkStatus(organizationId,issueTypeId, status.getId(),statusVO.getDefaultStatus(), statusVO.getTransferAll());
            }
        }
        return status;
    }
    @Override
    public void updateNodeObjectVersionNumber(Long organizationId, Long issueType, Long statusId, Long objectVersionNumber) {
        Long stateMachineId = queryStatusMachineId(organizationId, issueType);
        // 查询状态的node
        StatusMachineNodeDTO statusMachineNodeDTO = new StatusMachineNodeDTO();
        statusMachineNodeDTO.setStatusId(statusId);
        statusMachineNodeDTO.setStateMachineId(stateMachineId);
        statusMachineNodeDTO.setOrganizationId(organizationId);
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
    public Page<StatusSettingVO> statusTransformSettingList(Long organizationId, Long issueTypeId, PageRequest pageRequest, String param, String schemeCode) {
        // 获取状态加Id
        Long stateMachineId = queryStatusMachineId(organizationId, issueTypeId);
        Page<StatusSettingVO> page = PageHelper.doPageAndSort(pageRequest, () -> statusMapper.listStatusTransferByStateMachineId(organizationId, stateMachineId,param));
        List<StatusSettingVO> list = page.getContent();
        if (CollectionUtils.isEmpty(list)) {
            return new Page<>();
        }
        List<Long> statusIds = list.stream().map(StatusSettingVO::getId).collect(Collectors.toList());
        List<StatusTransferSettingVO> transferSettingVOS = statusTransferSettingService.listStatusTransfer(organizationId, issueTypeId, statusIds);
        List<StatusFieldSettingVO> statusFieldSettingVOS = statusFieldSettingService.listStatusFieldSetting(organizationId, issueTypeId, statusIds);
        List<StatusNoticeSettingVO> statusNoticeSettingVOS = statusNoticeSettingService.listStatusNoticeSetting(organizationId, issueTypeId, statusIds, schemeCode);
        List<StatusLinkageVO> linkageVOS = statusLinkageService.listStatusLinkage(organizationId , issueTypeId, statusIds);
        Map<Long, List<StatusBranchMergeSettingVO>> statusBranchMergeSettingMap =
                statusBranchMergeSettingService.listByOptions(0L, organizationId, issueTypeId, statusIds)
                        .stream()
                        .collect(Collectors.groupingBy(StatusBranchMergeSettingVO::getStatusId));
        //瀑布前置项配置
        Map<Long, List<PredecessorIssueStatusLinkageVO>> predecessorIssueMap = new HashMap<>();
        if (agileWaterfallService != null) {
            predecessorIssueMap = agileWaterfallService.listPredecessorIssueMapByIssueTypeAndStatusIds(0L, organizationId, issueTypeId, statusIds);
        }
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
            List<StatusBranchMergeSettingVO> statusBranchMergeSettingList = statusBranchMergeSettingMap.get(statusSettingVO.getId());
            if (!ObjectUtils.isEmpty(statusBranchMergeSettingList)) {
                statusSettingVO.setStatusBranchMergeSettingVO(statusBranchMergeSettingList.get(0));
            }
            statusSettingVO.setPredecessorIssueStatusLinkageVOS(predecessorIssueMap.getOrDefault(statusSettingVO.getId(), Collections.emptyList()));
        }
        page.setContent(list);
        return page;
    }

    @Override
    public void syncStatusMachineTemplate(ProjectEvent projectEvent, String applyType) {
        Long projectId = projectEvent.getProjectId();
        Long organizationId = projectEvent.getOrganizationId();
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
        if (CollectionUtils.isEmpty(issueTypeSchemeConfigDTOS)) {
            return;
        }
        List<Long> issueTypeIds = issueTypeSchemeConfigDTOS.stream().map(IssueTypeSchemeConfigDTO::getIssueTypeId).collect(Collectors.toList());
        Map<Long,Long> templateStatusMachineMap = new HashMap<>();
        handlerTemplateStatusMachineMap(templateStatusMachineMap, organizationId);
        Set<Long> templateIssueTypes = templateStatusMachineMap.keySet();
        for (Long issueTypeId : issueTypeIds) {
            StatusMachineSchemeConfigDTO config = queryStatusMachineSchemeConfig(stateMachineSchemeId,issueTypeId,organizationId);
            if (!ObjectUtils.isEmpty(config)) {
                continue;
            }
            if (templateIssueTypes.contains(issueTypeId)) {
                copyStatusMachine(projectId, organizationId, issueTypeId, stateMachineSchemeId, templateStatusMachineMap.get(issueTypeId), applyType, issueTypeIds);
            } else {
                stateMachineSchemeConfigService.queryStatusMachineBySchemeIdAndIssueType(organizationId, stateMachineSchemeId, issueTypeId);
                // 初始化状态和项目的关系
                initProjectStatusRel(organizationId, issueTypeId, stateMachineSchemeId, projectId);
            }
        }
    }

    private void initProjectStatusRel(Long organizationId, Long issueTypeId, Long schemeId, Long projectId) {
        StatusMachineSchemeConfigDTO statusMachineSchemeConfig = queryStatusMachineSchemeConfig(organizationId, issueTypeId, schemeId);
        if (ObjectUtils.isEmpty(statusMachineSchemeConfig)) {
           throw new CommonException("error.status.machine.scheme.config.not.exist");
        }
        List<StatusMachineNodeVO> machineNodeVOS = stateMachineNodeService.queryByStateMachineId(organizationId, statusMachineSchemeConfig.getStateMachineId(), false);
        List<IssueStatusVO> issueStatusVOS = issueStatusService.queryIssueStatusList(projectId);
        List<Long> projectStatusIds = new ArrayList<>();
        if (!CollectionUtils.isEmpty(issueStatusVOS)) {
            projectStatusIds.addAll(issueStatusVOS.stream().map(IssueStatusVO::getStatusId).collect(Collectors.toList()));
        }
        // 状态没有在项目下需要和项目建立关系
        for (StatusMachineNodeVO machineNodeVO : machineNodeVOS) {
            if (!projectStatusIds.contains(machineNodeVO.getStatusId())) {
                StatusDTO statusDTO = statusMapper.queryById(organizationId, machineNodeVO.getStatusId());
                if (!ObjectUtils.isEmpty(statusDTO)) {
                    StatusTemplateDTO statusTemplateDTO = new StatusTemplateDTO(organizationId, statusDTO.getId());
                    StatusTemplateDTO templateDTO = statusTemplateMapper.selectOne(statusTemplateDTO);
                    Boolean doneStatus = !ObjectUtils.isEmpty(statusDTO.getCode()) && Objects.equals("done", statusDTO.getType());
                    Boolean isCompleted = ObjectUtils.isEmpty(templateDTO) ? doneStatus : templateDTO.getTemplateCompleted();
                    IssueStatusDTO issueStatusDTO = new IssueStatusDTO();
                    issueStatusDTO.setName(statusDTO.getName());
                    issueStatusDTO.setProjectId(projectId);
                    issueStatusDTO.setCompleted(false);
                    issueStatusDTO.setEnable(false);
                    issueStatusDTO.setCategoryCode(statusDTO.getType());
                    issueStatusDTO.setStatusId(statusDTO.getId());
                    issueStatusDTO.setCompleted(isCompleted);
                    issueStatusService.insertIssueStatus(issueStatusDTO);
                }
            }
        }

    }

    @Override
    public Map<String,Boolean> checkConfigTemplate(Long organizationId) {
        Map<String,Boolean> configMap = new HashMap<>();
        // 查询是否配置看板模板
        Boolean boardTemplateConfig = false;
        BoardDTO boardDTO = new BoardDTO();
        boardDTO.setOrganizationId(organizationId);
        boardDTO.setProjectId(0L);
        List<BoardDTO> boardDTOS = boardMapper.select(boardDTO);
        if (!CollectionUtils.isEmpty(boardDTOS)) {
            boardTemplateConfig = true;
        }
        configMap.put("boardTemplateConfig", boardTemplateConfig);
        // 查询是否配置状态机模板
        Boolean statusMachineTemplateConfig = false;
        OrganizationConfigDTO organizationConfigDTO = querySchemeId(organizationId, SchemeType.STATE_MACHINE, SchemeApplyType.AGILE);
        if (!ObjectUtils.isEmpty(organizationConfigDTO)) {
            List<StatusMachineSchemeConfigVO> statusMachineSchemeConfigVOS = stateMachineSchemeConfigService.queryBySchemeId(false, organizationId, organizationConfigDTO.getSchemeId());
            List<IssueTypeVO> issueTypeVOS = issueTypeService.queryByOrgId(organizationId, null);
            String[] issueTypeCodes = {"feature", "backlog"};
            List<Long> issueTypeIds = issueTypeVOS.stream()
                    .filter(v -> Objects.equals("system", v.getSource()) && !Arrays.asList(issueTypeCodes).contains(v.getTypeCode()))
                    .map(IssueTypeVO::getId).collect(Collectors.toList());
            statusMachineSchemeConfigVOS = statusMachineSchemeConfigVOS.stream()
                    .filter(v -> issueTypeIds.contains(v.getIssueTypeId()))
                    .collect(Collectors.toList());
            if (!CollectionUtils.isEmpty(statusMachineSchemeConfigVOS)) {
                statusMachineTemplateConfig  = true;
            }
        } else {
            initOrganizationConfig(organizationId);
        }
        configMap.put("statusMachineTemplateConfig", statusMachineTemplateConfig);
        return configMap;
    }

    @Override
    public Boolean checkStatusMachineTemplate(Long organizationId, Long issueTypeId) {
        OrganizationConfigDTO organizationConfigDTO = querySchemeId(organizationId, SchemeType.STATE_MACHINE, SchemeApplyType.AGILE);
        if (!ObjectUtils.isEmpty(organizationConfigDTO)) {
            StatusMachineSchemeConfigDTO statusMachineSchemeConfigDTO = queryStatusMachineSchemeConfig(organizationId, issueTypeId, organizationConfigDTO.getSchemeId());
            return !ObjectUtils.isEmpty(statusMachineSchemeConfigDTO);
        } else {
            initOrganizationConfig(organizationId);
            return false;
        }
    }

    @Override
    public Long queryIssueTypeStatusMachineId(Long organizationId, Long issueTypeId) {
        OrganizationConfigDTO organizationConfigDTO = querySchemeId(organizationId, SchemeType.STATE_MACHINE, SchemeApplyType.AGILE);
        if (ObjectUtils.isEmpty(organizationConfigDTO)) {
            return null;
        }
        StatusMachineSchemeConfigDTO statusMachineSchemeConfigDTO = queryStatusMachineSchemeConfig(organizationId, issueTypeId, organizationConfigDTO.getSchemeId());
        return ObjectUtils.isEmpty(statusMachineSchemeConfigDTO) ? null : statusMachineSchemeConfigDTO.getStateMachineId();
    }

    @Override
    public List<IssueTypeVO> listIssueType(Long organizationId) {
        OrganizationConfigDTO organizationConfigDTO = querySchemeId(organizationId, SchemeType.STATE_MACHINE, SchemeApplyType.AGILE);
        if (ObjectUtils.isEmpty(organizationConfigDTO)) {
            return new ArrayList<>();
        }
        StatusMachineSchemeConfigDTO statusMachineSchemeConfigDTO = new StatusMachineSchemeConfigDTO();
        statusMachineSchemeConfigDTO.setSchemeId(organizationConfigDTO.getSchemeId());
        statusMachineSchemeConfigDTO.setOrganizationId(organizationId);
        List<StatusMachineSchemeConfigDTO> schemeConfigDTOList = statusMachineSchemeConfigMapper.select(statusMachineSchemeConfigDTO);
        if (CollectionUtils.isEmpty(schemeConfigDTOList)) {
            return new ArrayList<>();
        }
        List<Long> issueTypeIds = schemeConfigDTOList.stream().map(StatusMachineSchemeConfigDTO::getIssueTypeId).collect(Collectors.toList());
        List<IssueTypeDTO> issueTypeDTOS = issueTypeMapper.selectByCondition(Condition.builder(IssueTypeDTO.class)
                .where(Sqls.custom().andIn("id", issueTypeIds)).build());
        return modelMapper.map(issueTypeDTOS, new TypeToken<List<IssueTypeVO>>(){}.getType());
    }

    @Override
    public Boolean checkConfigured(Long organizationId, Long projectId) {
        ProjectConfigDTO projectConfigDTO = new ProjectConfigDTO();
        projectConfigDTO.setProjectId(projectId);
        return projectConfigMapper.select(projectConfigDTO).isEmpty();
    }

    @Override
    public StatusBranchMergeSettingVO queryStatusBranchMergeSetting(Long organizationId, Long issueTypeId, Long statusId) {
        return statusBranchMergeSettingService.query(0L, organizationId, issueTypeId, statusId);
    }

    @Override
    public void updateAutoTransform(Long organizationId, Long issueTypeId, Long statusId, Boolean autoTransform) {
        statusBranchMergeSettingService.updateAutoTransform(0L, organizationId, issueTypeId, statusId, autoTransform);
    }

    @Override
    public NodeSortVO updateSort(Long organizationId, Long statusMachineId, NodeSortVO nodeSortVO) {
        if (ObjectUtils.isEmpty(nodeSortVO.getNodeId())) {
            throw new CommonException("error.sort.node.null");
        }
        if (ObjectUtils.isEmpty(nodeSortVO.getOutSetId())) {
            throw new CommonException("error.outSetId.null");
        }
        // 对rank值为空的node进行处理
        stateMachineNodeService.handlerNullRankNode(organizationId, statusMachineId, null);
        // 进行排序
        stateMachineNodeService.sortNode(organizationId, statusMachineId, nodeSortVO, null);
        // 清除状态机实例
        instanceCache.cleanStateMachine(statusMachineId);
        return nodeSortVO;
    }

    private OrganizationConfigDTO initOrganizationConfig(Long organizationId){
        // 创建状态机方案
        Long schemeId = stateMachineSchemeService.initOrgDefaultStatusMachineScheme(organizationId);
        // 组织和方案建立关联
        OrganizationConfigDTO organizationConfigDTO = new OrganizationConfigDTO();
        organizationConfigDTO.setOrganizationId(organizationId);
        organizationConfigDTO.setSchemeId(schemeId);
        organizationConfigDTO.setSchemeType(SchemeType.STATE_MACHINE);
        organizationConfigDTO.setApplyType(SchemeApplyType.AGILE);
        baseInsert(organizationConfigDTO);
        return organizationConfigDTO;
    }

    private void handlerTemplateStatusMachineMap(Map<Long, Long> templateStatusMachineMap, Long organizationId) {
        OrganizationConfigDTO organizationConfigDTO = querySchemeId(organizationId, SchemeType.STATE_MACHINE, SchemeApplyType.AGILE);
        if (!ObjectUtils.isEmpty(organizationConfigDTO)) {
            List<StatusMachineSchemeConfigVO> statusMachineSchemeConfigVOS = stateMachineSchemeConfigService.queryBySchemeId(false, organizationId, organizationConfigDTO.getSchemeId());
            if (!CollectionUtils.isEmpty(statusMachineSchemeConfigVOS)) {
                for (StatusMachineSchemeConfigVO statusMachineSchemeConfig : statusMachineSchemeConfigVOS) {
                    templateStatusMachineMap.put(statusMachineSchemeConfig.getIssueTypeId(), statusMachineSchemeConfig.getStateMachineId());
                }
            }
        }
    }

    private void copyStatusMachine(Long projectId, Long organizationId, Long issueTypeId, Long schemeId, Long templateStatusMachine, String applyType, List<Long> issueTypeIds) {
        // 复制状态机模板
        Long stateMachine = stateMachineService.copyStateMachine(organizationId, templateStatusMachine, issueTypeId);
        insertSchemeConfig(organizationId, stateMachine, schemeId, issueTypeId);
        // 复制状态机模板设置的流转条件
        PageRequest pageRequest = new PageRequest();
        pageRequest.setPage(0);
        pageRequest.setSize(0);
        Page<StatusSettingVO> statusSettingVOS = statusTransformSettingList(organizationId, issueTypeId, pageRequest, null, "agile_issue");
        List<StatusSettingVO> content = statusSettingVOS.getContent();
        if (!CollectionUtils.isEmpty(content)) {
            // 查询项目下用户
            Page<UserVO> users = remoteIamOperator.queryUsersByProject(projectId, null, 0, 0);
            List<Long> userIds = new ArrayList<>();
            if (!CollectionUtils.isEmpty(users.getContent())) {
                userIds.addAll(users.getContent().stream().map(UserVO::getId).collect(Collectors.toList()));
            }
            List<IssueStatusVO> issueStatusVOS = issueStatusService.queryIssueStatusList(projectId);
            List<Long> projectStatusIds = new ArrayList<>();
            if (!CollectionUtils.isEmpty(issueStatusVOS)) {
                projectStatusIds.addAll(issueStatusVOS.stream().map(IssueStatusVO::getStatusId).collect(Collectors.toList()));
            }
            for (StatusSettingVO statusSettingVO : content) {
                // 状态没有在项目下需要和项目建立关系
                if (!projectStatusIds.contains(statusSettingVO.getId())) {
                    IssueStatusDTO issueStatusDTO = new IssueStatusDTO();
                    issueStatusDTO.setName(statusSettingVO.getName());
                    issueStatusDTO.setProjectId(projectId);
                    issueStatusDTO.setEnable(false);
                    issueStatusDTO.setCategoryCode(statusSettingVO.getType());
                    issueStatusDTO.setStatusId(statusSettingVO.getId());
                    StatusTemplateDTO statusTemplateDTO = new StatusTemplateDTO(organizationId, statusSettingVO.getId());
                    StatusTemplateDTO templateDTO = statusTemplateMapper.selectOne(statusTemplateDTO);
                    issueStatusDTO.setCompleted(ObjectUtils.isEmpty(templateDTO) ? Boolean.FALSE : templateDTO.getTemplateCompleted());
                    issueStatusService.insertIssueStatus(issueStatusDTO);
                }
                // 处理状态上设置的流转条件
                handlerTransfer(statusSettingVO, issueTypeId, projectId, userIds, applyType, issueTypeIds);
            }
        }
    }

    private void handlerTransfer(StatusSettingVO statusSettingVO, Long issueTypeId, Long projectId, List<Long> userIds, String applyType, List<Long> issueTypeIds) {
        Long statusId = statusSettingVO.getId();
        Long objectVersionNumber = 1L;
        // 更新属性
        List<StatusFieldSettingVO> statusFieldSettingVOS = statusSettingVO.getStatusFieldSettingVOS();
        if (!CollectionUtils.isEmpty(statusFieldSettingVOS)) {
            List<StatusFieldSettingVO> statusFieldSetting = new ArrayList<>();
            filterStatusFieldSetting(projectId, statusFieldSettingVOS, statusFieldSetting, userIds);
            statusFieldSettingService.createOrUpdate(projectId, issueTypeId, statusId, objectVersionNumber, applyType, statusFieldSetting);
            objectVersionNumber++;
        }
        // 状态联动
        List<StatusLinkageVO> statusLinkageVOS = statusSettingVO.getStatusLinkageVOS();
        if (!CollectionUtils.isEmpty(statusLinkageVOS)) {
            List<StatusLinkageVO> statusLinkage = new ArrayList<>();
            handlerStatusLinkage(projectId, statusLinkageVOS, issueTypeIds, statusLinkage);
            if (!CollectionUtils.isEmpty(statusLinkage)) {
                statusLinkageService.createOrUpdate(projectId, issueTypeId, statusId, objectVersionNumber, applyType, statusLinkage);
                objectVersionNumber++;
            }
        }
        // 瀑布-状态联动-关联工作项联动
        copyPredecessorIssueStatusLinkage(statusSettingVO, projectId, issueTypeId, statusId);
        // 通知设置
        List<StatusNoticeSettingVO> statusNoticeSettingVOS = statusSettingVO.getStatusNoticeSettingVOS();
        if (!CollectionUtils.isEmpty(statusNoticeSettingVOS)) {
            StatusNoticeSettingVO statusNoticeSettingVO = statusNoticeSettingVOS.get(0);
            Set<Long> userIdList = statusNoticeSettingVO.getUserIdList();
            List<String> noticeTypeList = statusNoticeSettingVO.getNoticeTypeList();
            statusNoticeSettingVO.setObjectVersionNumber(objectVersionNumber);
            statusNoticeSettingVO.setOrganizationId(null);
            statusNoticeSettingVO.setProjectId(projectId);
            if (!CollectionUtils.isEmpty(userIdList)) {
                Set<Long> users = userIdList.stream()
                        .filter(userIds::contains)
                        .collect(Collectors.toSet());
                if (!CollectionUtils.isEmpty(noticeTypeList) || !CollectionUtils.isEmpty(users)) {
                    statusNoticeSettingVO.setUserIdList(users);
                    statusNoticeSettingService.save(projectId, statusNoticeSettingVO, applyType);
                    objectVersionNumber++;
                }
            } else {
                statusNoticeSettingService.save(projectId, statusNoticeSettingVO, applyType);
                objectVersionNumber++;
            }
        }
        // 状态流转
        List<StatusTransferSettingVO> statusTransferSettingVOS = statusSettingVO.getStatusTransferSettingVOS();
        if (!CollectionUtils.isEmpty(statusTransferSettingVOS)) {
            List<StatusTransferSettingCreateVO> list = handlerTransfer(statusTransferSettingVOS, userIds);
            statusTransferSettingService.createOrUpdate(projectId, issueTypeId, statusId, objectVersionNumber, applyType, list);
        }
        // 分支合并状态流转
        StatusBranchMergeSettingVO statusBranchMergeSetting = statusSettingVO.getStatusBranchMergeSettingVO();
        if (statusBranchMergeSetting != null) {
            statusBranchMergeSettingService.updateAutoTransform(
                    projectId,
                    ConvertUtil.getOrganizationId(projectId),
                    statusBranchMergeSetting.getIssueTypeId(),
                    statusBranchMergeSetting.getStatusId(),
                    statusBranchMergeSetting.getAutoTransform());
        }
    }

    private void copyPredecessorIssueStatusLinkage(StatusSettingVO statusSetting,
                                                   Long projectId,
                                                   Long issueTypeId,
                                                   Long statusId) {
        List<PredecessorIssueStatusLinkageVO> predecessorIssueStatusLinkage = statusSetting.getPredecessorIssueStatusLinkageVOS();
        if (!CollectionUtils.isEmpty(predecessorIssueStatusLinkage) && agileWaterfallService != null) {
            agileWaterfallService.copyPredecessorIssueStatusLinkage(predecessorIssueStatusLinkage, projectId, issueTypeId, statusId);
        }
    }

    private List<StatusTransferSettingCreateVO> handlerTransfer(List<StatusTransferSettingVO> sourceStatusTransferSettings,
                                                                List<Long> userIdInProject) {
        List<StatusTransferSettingCreateVO> result = new ArrayList<>();
        Map<String, List<StatusTransferSettingVO>> map =
                sourceStatusTransferSettings.stream().collect(Collectors.groupingBy(StatusTransferSettingVO::getUserType));
        for (Map.Entry<String, List<StatusTransferSettingVO>> entry : map.entrySet()) {
            StatusTransferSettingCreateVO settingCreateVO = new StatusTransferSettingCreateVO();
            String userType = entry.getKey();
            settingCreateVO.setType(userType);
            List<StatusTransferSettingVO> settings = entry.getValue();
            if (StatusTransferType.SPECIFIER.equals(userType)) {
                List<Long> userIds =
                        settings.stream()
                                .map(StatusTransferSettingVO::getUserId)
                                .filter(userIdInProject::contains)
                                .collect(Collectors.toList());
                if (!CollectionUtils.isEmpty(userIds)) {
                    settingCreateVO.setUserIds(userIds);
                    result.add(settingCreateVO);
                }
            } else if (StatusTransferType.ROLE.equals(userType)) {
                List<Long> roleIds =
                        settings.stream()
                                .map(StatusTransferSettingVO::getUserId)
                                .collect(Collectors.toList());
                if (!CollectionUtils.isEmpty(roleIds)) {
                    settingCreateVO.setUserIds(roleIds);
                    result.add(settingCreateVO);
                }
            } else if (StatusTransferType.OTHER.equals(userType)) {
                StatusTransferSettingVO other = settings.get(0);
                settingCreateVO.setVerifySubissueCompleted(other.getVerifySubissueCompleted());
                result.add(settingCreateVO);
            } else {
                result.add(settingCreateVO);
            }
        }
        return result;
    }

    private void handlerStatusLinkage(Long projectId, List<StatusLinkageVO> statusLinkageVOS, List<Long> issueTypeIds, List<StatusLinkageVO> statusLinkage) {
        for (StatusLinkageVO statusLinkageVO : statusLinkageVOS) {
            if(issueTypeIds.contains(statusLinkageVO.getParentIssueTypeId())){
                statusLinkageVO.setId(null);
                statusLinkageVO.setProjectId(projectId);
                statusLinkage.add(statusLinkageVO);
            }
        }
    }

    private void filterStatusFieldSetting(Long projectId, List<StatusFieldSettingVO> statusFieldSettingVOS, List<StatusFieldSettingVO> statusFieldSetting, List<Long> userIds) {
        for (StatusFieldSettingVO statusFieldSettingVO : statusFieldSettingVOS) {
            String fieldType = statusFieldSettingVO.getFieldType();
            statusFieldSettingVO.setId(null);
            statusFieldSettingVO.setProjectId(projectId);
            if (Objects.equals(FieldType.MULTI_MEMBER, fieldType) || Objects.equals(FieldType.MEMBER, fieldType)) {
                List<StatusFieldValueSettingDTO> fieldValueSettingDTOS = new ArrayList<>();
                for (StatusFieldValueSettingDTO statusFieldValueSetting : statusFieldSettingVO.getFieldValueList()) {
                    statusFieldValueSetting.setId(null);
                    statusFieldValueSetting.setProjectId(projectId);
                    if (ObjectUtils.isEmpty(statusFieldValueSetting.getUserId()) || (!ObjectUtils.isEmpty(statusFieldValueSetting.getUserId()) && userIds.contains(statusFieldValueSetting.getUserId()))) {
                        fieldValueSettingDTOS.add(statusFieldValueSetting);
                    }
                }
                if(!CollectionUtils.isEmpty(fieldValueSettingDTOS)){
                    statusFieldSettingVO.setFieldValueList(fieldValueSettingDTOS);
                    statusFieldSetting.add(statusFieldSettingVO);
                }
            } else {
                List<StatusFieldValueSettingDTO> fieldValueSettingDTOS = new ArrayList<>();
                for (StatusFieldValueSettingDTO statusFieldValueSetting : statusFieldSettingVO.getFieldValueList()) {
                    statusFieldValueSetting.setId(null);
                    statusFieldValueSetting.setProjectId(projectId);
                    fieldValueSettingDTOS.add(statusFieldValueSetting);
                }
                statusFieldSettingVO.setFieldValueList(fieldValueSettingDTOS);
                statusFieldSetting.add(statusFieldSettingVO);
            }
        }
    }

    private void insertSchemeConfig(Long organizationId, Long stateMachineId, Long stateMachineSchemeId, Long issueTypeId) {
        StatusMachineSchemeConfigDTO configDTO = new StatusMachineSchemeConfigDTO();
        configDTO.setOrganizationId(organizationId);
        configDTO.setSchemeId(stateMachineSchemeId);
        configDTO.setStateMachineId(stateMachineId);
        configDTO.setDefault(false);
        configDTO.setSequence(0);
        configDTO.setIssueTypeId(issueTypeId);
        if (statusMachineSchemeConfigMapper.insert(configDTO) != 1) {
            throw new CommonException("error.insert.state.machine.scheme.config");
        }
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

    private StatusMachineNodeDTO checkStatusLink(Long organizationId, Long issueTypeId, Long nodeId) {
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

        // 校验当前node的状态是否与其他状态有联动
        List<StatusLinkageDTO> linkExistList =
                statusLinkageMapper.selectExistStatusLink(organizationId, machineNodeDTO.getStatusId(), issueTypeId);
        if (!CollectionUtils.isEmpty(linkExistList)){
            throw new CommonException("error.status.status_link_exist");
        }
        Sqls existCondition = Sqls.custom().andEqualTo("organizationId", organizationId).andEqualTo("projectId", 0L).andEqualTo("statusId", currentStatusId).andEqualTo("issueTypeId", issueTypeId);
        // 校验是否关联流转条件
        List<StatusTransferSettingDTO> transferExist =
                statusTransferSettingMapper.selectByCondition(Condition.builder(StatusTransferSettingDTO.class)
                        .andWhere(existCondition).build());
        if (org.apache.commons.collections4.CollectionUtils.isNotEmpty(transferExist)){
            throw new CommonException("error.status.status_transfer_exist");
        }
        // 校验是否关联属性字段
        List<StatusFieldSettingDTO> statusFieldExist =
                statusFieldSettingMapper.selectByCondition(Condition.builder(StatusFieldSettingDTO.class)
                        .andWhere(existCondition).build());
        if (org.apache.commons.collections4.CollectionUtils.isNotEmpty(statusFieldExist)){
            throw new CommonException("error.status.status_field_exist");
        }
        // 校验是否存在通知设置
        List<StatusNoticeSettingDTO> statusNoticeExist =
                statusNoticeSettingMapper.selectByCondition(Condition.builder(StatusNoticeSettingDTO.class)
                        .andWhere(existCondition).build());
        if (org.apache.commons.collections4.CollectionUtils.isNotEmpty(statusNoticeExist)){
            throw new CommonException("error.status.status_notice_exist");
        }
        return  machineNodeDTO;
    }

    @Override
    public Long queryStatusMachineId(Long organizationId, Long issueTypeId) {
        OrganizationConfigDTO organizationConfigDTO = querySchemeId(organizationId, "scheme_state_machine", SchemeApplyType.AGILE);
        if (ObjectUtils.isEmpty(organizationConfigDTO)) {
            Long schemeId = stateMachineSchemeService.initOrgDefaultStatusMachineScheme(organizationId);
            // 组织和方案建立关联
            OrganizationConfigDTO organizationConfig = new OrganizationConfigDTO();
            organizationConfig.setOrganizationId(organizationId);
            organizationConfig.setSchemeId(schemeId);
            organizationConfig.setSchemeType("scheme_state_machine");
            organizationConfig.setApplyType("agile");
            baseInsert(organizationConfig);
            organizationConfigDTO = organizationConfig;
        }
        StatusMachineSchemeConfigDTO statusMachineSchemeConfig = queryStatusMachineSchemeConfig(organizationId, issueTypeId, organizationConfigDTO.getSchemeId());
        if (ObjectUtils.isEmpty(statusMachineSchemeConfig)) {
            throw new CommonException("error.status.machine.template.scheme.config.null");
        }
        return statusMachineSchemeConfig.getStateMachineId();
    }

    private StatusMachineSchemeConfigDTO queryStatusMachineSchemeConfig(Long organizationId, Long issueTypeId, Long schemeId) {
        StatusMachineSchemeConfigDTO statusMachineSchemeConfigDTO = new StatusMachineSchemeConfigDTO();
        statusMachineSchemeConfigDTO.setIssueTypeId(issueTypeId);
        statusMachineSchemeConfigDTO.setSchemeId(schemeId);
        statusMachineSchemeConfigDTO.setOrganizationId(organizationId);
        List<StatusMachineSchemeConfigDTO> schemeConfigDTOList = statusMachineSchemeConfigMapper.select(statusMachineSchemeConfigDTO);
        if (CollectionUtils.isEmpty(schemeConfigDTOList)) {
            return null;
        } else {
            return schemeConfigDTOList.get(0);
        }
    }

    private void baseInsert(OrganizationConfigDTO organizationConfigDTO) {
        if (organizationConfigMapper.insertSelective(organizationConfigDTO) != 1) {
            throw new CommonException("error.insert.organization.config");
        }
    }

    private void initStatusMachine(Long organizationId, Long issueTypeId, Long schemeId) {
        OrganizationInfoVO organization = remoteIamOperator.query(organizationId);
        IssueTypeDTO issueTypeDTO = issueTypeMapper.selectByPrimaryKey(issueTypeId);
        if (ObjectUtils.isEmpty(issueTypeDTO)) {
            throw new CommonException("error.issue.type.not.exist");
        }
        StatusMachineDTO statusMachine = new StatusMachineDTO();
        statusMachine.setOrganizationId(organizationId);
        String name = "组织" + organization.getTenantNum() + issueTypeDTO.getName() + "状态机【敏捷】";
        statusMachine.setName(name);
        statusMachine.setDescription(name);
        statusMachine.setStatus(StateMachineStatus.ACTIVE);
        statusMachine.setDefault(false);
        if (statusMachineMapper.insert(statusMachine) != 1) {
            throw new CommonException("error.insert.status.machine");
        }
        String applyType = Objects.equals("feature", issueTypeDTO.getTypeCode()) ? "program" : "default";
        initService.createStateMachineDetail(organizationId, statusMachine.getId(), applyType);
        StatusMachineSchemeConfigDTO defaultConfig = new StatusMachineSchemeConfigDTO();
        defaultConfig.setStateMachineId(statusMachine.getId());
        defaultConfig.setIssueTypeId(issueTypeId);
        defaultConfig.setSequence(0);
        defaultConfig.setSchemeId(schemeId);
        defaultConfig.setOrganizationId(organizationId);
        defaultConfig.setDefault(false);
        int isInsert = statusMachineSchemeConfigMapper.insert(defaultConfig);
        if (isInsert < 1) {
            throw new CommonException("error.stateMachineSchemeConfig.insert");
        }
    }

    public OrganizationConfigDTO querySchemeId(Long organizationId, String schemeType, String applyType) {
        OrganizationConfigDTO organizationConfigDTO = new OrganizationConfigDTO();
        organizationConfigDTO.setOrganizationId(organizationId);
        organizationConfigDTO.setSchemeType(schemeType);
        organizationConfigDTO.setApplyType(applyType);
        return organizationConfigMapper.selectOne(organizationConfigDTO);
    }
}
