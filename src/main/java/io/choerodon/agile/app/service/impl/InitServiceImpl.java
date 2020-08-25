package io.choerodon.agile.app.service.impl;

import io.choerodon.agile.api.vo.event.ProjectEvent;
import io.choerodon.agile.api.vo.event.StatusPayload;
import io.choerodon.agile.app.service.BoardService;
import io.choerodon.agile.app.service.InitService;
import io.choerodon.agile.app.service.StateMachineService;
import io.choerodon.agile.infra.dto.*;
import io.choerodon.agile.infra.enums.*;
import io.choerodon.agile.infra.mapper.*;
import io.choerodon.core.exception.CommonException;
import io.choerodon.core.oauth.DetailsHelper;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * @author shinan.chen
 * @date 2018/10/15
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class InitServiceImpl implements InitService {

    private static final String ERROR_STATEMACHINE_CREATE = "error.stateMachine.create";
    private static final String DEFAULT_BOARD = "默认看板";

    @Autowired
    private StatusMapper statusMapper;
    @Autowired
    private StateMachineNodeDraftMapper nodeDraftMapper;
    @Autowired
    private StateMachineService stateMachineService;
    @Autowired
    private StatusMachineMapper statusMachineMapper;
    @Autowired
    private StateMachineTransformDraftMapper transformDraftMapper;
    @Autowired
    private BoardService boardService;
    @Autowired
    private ModelMapper modelMapper;
    @Autowired
    private StatusMachineNodeMapper statusMachineNodeMapper;
    @Autowired
    private StatusMachineTransformMapper statusMachineTransformMapper;

    @Override
    public synchronized List<StatusDTO> initStatus(Long organizationId) {
        List<StatusDTO> initStatuses = new ArrayList<>();
        for (InitStatus initStatus : InitStatus.values()) {
            StatusDTO status = new StatusDTO();
            status.setOrganizationId(organizationId);
            status.setCode(initStatus.getCode());
            List<StatusDTO> statuses = statusMapper.select(status);
            if (statuses.isEmpty()) {
                status.setName(initStatus.getName());
                status.setDescription(initStatus.getName());
                status.setType(initStatus.getType());
                if (statusMapper.insert(status) != 1) {
                    throw new CommonException("error.initStatus.create");
                }
                initStatuses.add(status);
            } else {
                initStatuses.add(statuses.get(0));
            }
        }
        return initStatuses;
    }

    @Override
    public Long createStateMachineWithCreateProject(Long organizationId, String applyType, ProjectEvent projectEvent) {
        Long stateMachineId = null;
        if (applyType.equals(SchemeApplyType.AGILE)) {
            stateMachineId = initAGStateMachine(organizationId, projectEvent);
        } else if (applyType.equals(SchemeApplyType.TEST)) {
            stateMachineId = initTEStateMachine(organizationId, projectEvent);
        }
        return stateMachineId;
    }

    @Override
    public Long initDefaultStateMachine(Long organizationId) {
        //初始化默认状态机
        StatusMachineDTO stateMachine = new StatusMachineDTO();
        stateMachine.setOrganizationId(organizationId);
        stateMachine.setName("默认状态机");
        stateMachine.setDescription("默认状态机");
        stateMachine.setStatus(StateMachineStatus.CREATE);
        stateMachine.setDefault(true);
        List<StatusMachineDTO> selects = statusMachineMapper.select(stateMachine);
        Long stateMachineId;
        if (selects.isEmpty()) {
            if (statusMachineMapper.insert(stateMachine) != 1) {
                throw new CommonException(ERROR_STATEMACHINE_CREATE);
            }
            //创建状态机节点和转换
            createStateMachineDetail(organizationId, stateMachine.getId(), "default");
            stateMachineId = stateMachine.getId();
        } else {
            stateMachineId = selects.get(0).getId();
        }
        return stateMachineId;
    }

    @Override
    public Long initAGStateMachine(Long organizationId, ProjectEvent projectEvent) {
        String projectCode = projectEvent.getProjectCode();
        //初始化状态机
        StatusMachineDTO stateMachine = new StatusMachineDTO();
        stateMachine.setOrganizationId(organizationId);
        stateMachine.setName(projectCode + "默认状态机【敏捷】");
        stateMachine.setDescription(projectCode + "默认状态机【敏捷】");
        stateMachine.setStatus(StateMachineStatus.ACTIVE);
        stateMachine.setDefault(false);
        if (statusMachineMapper.insert(stateMachine) != 1) {
            throw new CommonException(ERROR_STATEMACHINE_CREATE);
        }
        //创建状态机节点和转换
        createStateMachineDetail(organizationId, stateMachine.getId(), SchemeApplyType.AGILE);
        //发布状态机
        Long stateMachineId = stateMachine.getId();
//        stateMachineService.deploy(organizationId, stateMachineId, false);
        //敏捷创建完状态机后需要到敏捷创建列
        List<StatusPayload> statusPayloads = statusMachineMapper.getStatusBySmId(projectEvent.getProjectId(), stateMachineId);
        boardService.initBoard(projectEvent.getProjectId(), DEFAULT_BOARD, statusPayloads);
        return stateMachineId;
    }

    @Override
    public Long initTEStateMachine(Long organizationId, ProjectEvent projectEvent) {
        String projectCode = projectEvent.getProjectCode();
        //初始化状态机
        StatusMachineDTO stateMachine = new StatusMachineDTO();
        stateMachine.setOrganizationId(organizationId);
        stateMachine.setName(projectCode + "默认状态机【测试】");
        stateMachine.setDescription(projectCode + "默认状态机【测试】");
        stateMachine.setStatus(StateMachineStatus.ACTIVE);
        stateMachine.setDefault(false);
        if (statusMachineMapper.insert(stateMachine) != 1) {
            throw new CommonException(ERROR_STATEMACHINE_CREATE);
        }
        //创建状态机节点和转换
        createStateMachineDetail(organizationId, stateMachine.getId(), SchemeApplyType.TEST);
        //发布状态机
        Long stateMachineId = stateMachine.getId();
//        stateMachineService.deploy(organizationId, stateMachineId, false);
        return stateMachineId;
    }

    /**
     * 创建状态机节点和转换
     *
     * @param organizationId
     * @param stateMachineId
     */
    @Override
    public void createStateMachineDetail(Long organizationId, Long stateMachineId, String applyType) {
        StatusDTO select = new StatusDTO();
        select.setOrganizationId(organizationId);
        List<StatusDTO> initStatuses = statusMapper.select(select);
        //老的组织没有相关数据要重新创建
        initStatuses = initOrganization(organizationId, initStatuses);
        //初始化节点
        Map<String, StatusMachineNodeDTO> nodeMap = new HashMap<>();
        Map<String, StatusDTO> statusMap = initStatuses.stream().filter(x -> x.getCode() != null).collect(Collectors.toMap(StatusDTO::getCode, x -> x, (code1, code2) -> code1));
        handleNode(organizationId, stateMachineId, applyType, nodeMap, statusMap);

        //初始化转换
        List<StatusMachineNodeDTO> nodeDTOS = nodeMap.entrySet().stream().map(Map.Entry::getValue).filter(v -> !v.getStatusId().equals(0L)).collect(Collectors.toList());
        // 初始化节点
        Long userId = DetailsHelper.getUserDetails().getUserId();
        List<StatusMachineTransformDTO> list = new ArrayList<>();
        StatusMachineTransformDTO transform = new StatusMachineTransformDTO();
        transform.setStateMachineId(stateMachineId);
        transform.setName("初始化");
        transform.setType(TransformType.INIT);
        transform.setConditionStrategy(TransformConditionStrategy.ALL);
        transform.setStartNodeId(0L);
        transform.setEndNodeId(nodeDTOS.get(0).getId());
        transform.setCreatedBy(userId);
        transform.setCreatedBy(userId);
        transform.setOrganizationId(organizationId);
        list.add(transform);
        Map<Long, StatusDTO> statusDTOMap = initStatuses.stream().filter(x -> x.getCode() != null).collect(Collectors.toMap(StatusDTO::getId, x -> x, (code1, code2) -> code1));
        for (StatusMachineNodeDTO startNode:nodeDTOS) {
            List<StatusMachineNodeDTO> endNodes = nodeDTOS.stream().filter(v -> !startNode.getId().equals(v.getId())).collect(Collectors.toList());
            for (StatusMachineNodeDTO endNode :endNodes) {
                StatusMachineTransformDTO transformDTO = new StatusMachineTransformDTO();
                transformDTO.setStateMachineId(stateMachineId);
                String startStatusName = statusDTOMap.get(startNode.getStatusId()).getName();
                String endStatusName = statusDTOMap.get(endNode.getStatusId()).getName();
                transformDTO.setName(startStatusName + "转换到"+ endStatusName);
                transformDTO.setType(TransformType.CUSTOM);
                transformDTO.setConditionStrategy(TransformConditionStrategy.ALL);
                transformDTO.setStartNodeId(startNode.getId());
                transformDTO.setEndNodeId(endNode.getId());
                transformDTO.setCreatedBy(userId);
                transformDTO.setOrganizationId(organizationId);
                transformDTO.setLastUpdatedBy(userId);
                list.add(transformDTO);
            }
        }
        statusMachineTransformMapper.batchInsert(list);
    }

    private void handleNode(Long organizationId, Long stateMachineId, String applyType, Map<String, StatusMachineNodeDTO> nodeMap, Map<String, StatusDTO> statusMap) {
        for (InitNode initNode : InitNode.list(applyType)) {
            StatusMachineNodeDTO node = new StatusMachineNodeDTO();
            node.setStateMachineId(stateMachineId);
            if (initNode.getType().equals(NodeType.START)) {
                node.setStatusId(0L);
            } else {
                node.setStatusId(statusMap.get(initNode.getCode()).getId());
            }
            node.setPositionX(initNode.getPositionX());
            node.setPositionY(initNode.getPositionY());
            node.setWidth(initNode.getWidth());
            node.setHeight(initNode.getHeight());
            node.setType(initNode.getType());
            node.setOrganizationId(organizationId);
            int isNodeInsert = statusMachineNodeMapper.insert(node);
            if (isNodeInsert != 1) {
                throw new CommonException("error.stateMachineNode.create");
            }
            nodeMap.put(initNode.getCode(), node);
        }
    }

    private List<StatusDTO> initOrganization(Long organizationId, List<StatusDTO> initStatuses) {
        if (initStatuses == null || initStatuses.isEmpty()) {
            //初始化状态
            initStatus(organizationId);
            //初始化默认状态机
            initDefaultStateMachine(organizationId);
            StatusDTO select = new StatusDTO();
            select.setOrganizationId(organizationId);
            return statusMapper.select(select);
        } else {
            return initStatuses;
        }
    }
}
