package io.choerodon.agile.app.service.impl;

import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.app.service.*;
import io.choerodon.agile.infra.cache.InstanceCache;
import io.choerodon.agile.infra.dto.*;
import io.choerodon.agile.infra.enums.NodeType;
import io.choerodon.agile.infra.enums.StateMachineStatus;
import io.choerodon.agile.infra.enums.TransformConditionStrategy;
import io.choerodon.agile.infra.enums.TransformType;
import io.choerodon.agile.infra.feign.BaseFeignClient;
import io.choerodon.agile.infra.feign.vo.OrganizationInfoVO;
import io.choerodon.agile.infra.mapper.*;
import io.choerodon.agile.infra.utils.RankUtil;
import io.choerodon.core.domain.Page;
import io.choerodon.core.exception.CommonException;
import io.choerodon.mybatis.pagehelper.PageHelper;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;
import org.apache.commons.lang3.StringUtils;
import org.hzero.core.base.BaseConstants;
import org.hzero.mybatis.domian.Condition;
import org.hzero.mybatis.util.Sqls;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.Assert;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;

import java.util.*;
import java.util.stream.Collectors;

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
    private BaseFeignClient baseFeignClient;
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

    @Override
    public OrganizationConfigDTO initStatusMachineTemplate(Long organizationId, Long issueTypeId) {
        OrganizationConfigDTO configDTO = querySchemeId(organizationId, "scheme_state_machine", "agile");
        if (ObjectUtils.isEmpty(configDTO)) {
            // 创建状态机方案
            Long schemeId = stateMachineSchemeService.initOrgDefaultStatusMachineScheme(organizationId);
            // 初始化问题类型的状态机
            initStatusMachine(organizationId, issueTypeId, schemeId);
            // 组织和方案建立关联
            OrganizationConfigDTO organizationConfigDTO = new OrganizationConfigDTO();
            organizationConfigDTO.setOrganizationId(organizationId);
            organizationConfigDTO.setSchemeId(schemeId);
            organizationConfigDTO.setSchemeType("scheme_state_machine");
            organizationConfigDTO.setApplyType("agile");
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
        List<StatusAndTransformVO> statusVOS = statusService.queryStatusByStateMachineId(organizationId, stateMachineId);
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
    }

    @Override
    public StatusMachineNodeVO linkStatus(Long organizationId, Long issueTypeId, Long statusId, Boolean defaultStatus, Boolean transferAll) {
        StatusVO statusVO = statusService.queryStatusById(organizationId, statusId);
        stateMachineNodeService.handlerNullRankNode(organizationId, statusId, "");
        Long stateMachineId = queryStatusMachineId(organizationId, issueTypeId);
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
            stateMachineNode.setRank(maxRank);
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
        page.setContent(list);
        return page;
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
    private Long queryStatusMachineId(Long organizationId, Long issueTypeId){
        OrganizationConfigDTO organizationConfigDTO = initStatusMachineTemplate(organizationId, issueTypeId);
        StatusMachineSchemeConfigDTO statusMachineSchemeConfig = queryStatusMachineSchemeConfig(organizationId, issueTypeId, organizationConfigDTO.getSchemeId());
        if (ObjectUtils.isEmpty(statusMachineSchemeConfig)) {
            throw new CommonException("error.status.machine.scheme.config.null");
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
        OrganizationInfoVO organization = baseFeignClient.query(organizationId).getBody();
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
