package io.choerodon.agile.app.service.impl;

import java.util.*;
import java.util.stream.Collectors;

import org.modelmapper.ModelMapper;
import org.modelmapper.TypeToken;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.statemachine.StateContext;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.ObjectUtils;

import io.choerodon.agile.api.vo.StateMachineConfigVO;
import io.choerodon.agile.api.vo.StatusMachineTransformVO;
import io.choerodon.agile.api.vo.event.TransformInfo;
import io.choerodon.agile.app.service.InstanceService;
import io.choerodon.agile.app.service.StateMachineConfigService;
import io.choerodon.agile.app.service.StateMachineTransformService;
import io.choerodon.agile.infra.dto.StatusMachineDTO;
import io.choerodon.agile.infra.dto.StatusMachineNodeDTO;
import io.choerodon.agile.infra.dto.StatusMachineTransformDTO;
import io.choerodon.agile.infra.enums.ConfigType;
import io.choerodon.agile.infra.enums.NodeType;
import io.choerodon.agile.infra.factory.MachineFactory;
import io.choerodon.agile.infra.mapper.StatusMachineMapper;
import io.choerodon.agile.infra.mapper.StatusMachineNodeMapper;
import io.choerodon.agile.infra.mapper.StatusMachineTransformMapper;
import io.choerodon.agile.infra.statemachineclient.dto.ExecuteResult;
import io.choerodon.agile.infra.statemachineclient.dto.InputDTO;
import io.choerodon.agile.infra.statemachineclient.service.ClientService;
import io.choerodon.core.exception.CommonException;

/**
 * @author shinan.chen
 * @date 2018/9/18
 */
@Service("instanceService")
@Transactional(rollbackFor = Exception.class)
public class InstanceServiceImpl implements InstanceService {

    private static final Logger LOGGER = LoggerFactory.getLogger(InstanceServiceImpl.class);
    private static final String EXCEPTION = "Exception:{}";
    @Autowired
    private StatusMachineNodeMapper nodeDeployMapper;
    @Autowired
    private StateMachineConfigService configService;
    @Autowired
    private StateMachineTransformService transformService;
    @Autowired
    private StatusMachineTransformMapper transformMapper;
    @Autowired
    private MachineFactory machineFactory;
    @Autowired
    private StatusMachineMapper statusMachineMapper;
    @Autowired
    private ModelMapper modelMapper;
    @Autowired
    @Qualifier("clientService")
    private ClientService stateMachineClientService;

    @Override
    public ExecuteResult startInstance(Long organizationId, String serviceCode, Long stateMachineId, InputDTO inputDTO) {
        StatusMachineDTO stateMachine = statusMachineMapper.queryById(organizationId, stateMachineId);
        if (stateMachine == null) {
            throw new CommonException("error.stateMachine.notFound");
        }
        ExecuteResult executeResult;
        try {
            executeResult = machineFactory.startInstance(organizationId, serviceCode, stateMachineId, inputDTO);
        } catch (Exception e) {
            LOGGER.error(e.getMessage(), e);
            executeResult = new ExecuteResult(false, null, "创建状态机实例失败");
        }
        return executeResult;
    }

    @Override
    public Long queryInitStatusId(Long organizationId, Long stateMachineId) {
        StatusMachineNodeDTO select = new StatusMachineNodeDTO();
        select.setOrganizationId(organizationId);
        select.setStateMachineId(stateMachineId);
        select.setType(NodeType.INIT);
        List<StatusMachineNodeDTO> nodes = nodeDeployMapper.select(select);
        if (nodes.isEmpty()) {
            throw new CommonException("error.queryInitStatusId.notFound");
        }
        return nodes.get(0).getStatusId();
    }

    @Override
    public ExecuteResult executeTransform(Long organizationId, String serviceCode, Long stateMachineId, Long currentStatusId, Long transformId, InputDTO inputDTO) {
        return machineFactory.executeTransform(organizationId, serviceCode, stateMachineId, currentStatusId, transformId, inputDTO);
    }

    @Override
    public List<TransformInfo> queryListTransform(Long organizationId, String serviceCode, Long stateMachineId, Long instanceId, Long statusId) {
        Boolean isNeedFilter = false;
        List<StatusMachineTransformDTO> stateMachineTransforms = transformService.queryListByStatusIdByDeploy(organizationId, stateMachineId, statusId);
        //获取节点信息
        List<StatusMachineNodeDTO> nodes = nodeDeployMapper.selectByStateMachineId(stateMachineId);
        List<StateMachineConfigVO> configs = configService.queryDeployByTransformIds(organizationId, ConfigType.CONDITION, stateMachineTransforms.stream().map(StatusMachineTransformDTO::getId).collect(Collectors.toList()));
        Map<Long, Long> nodeMap = nodes.stream().collect(Collectors.toMap(StatusMachineNodeDTO::getId, StatusMachineNodeDTO::getStatusId));
        Map<Long, String> nodeRankMap = new HashMap<>();
        for (StatusMachineNodeDTO node : nodes) {
            nodeRankMap.put(node.getId(), node.getRank());
        }
        Map<Long, List<StateMachineConfigVO>> configMaps = configs.stream().collect(Collectors.groupingBy(StateMachineConfigVO::getTransformId));
        List<TransformInfo> transformInfos = new ArrayList<>(stateMachineTransforms.size());
        Boolean hasRankNull = false;
        for (StatusMachineTransformDTO transform : stateMachineTransforms) {
            TransformInfo transformInfo = modelMapper.map(transform, TransformInfo.class);
            transformInfo.setStartStatusId(nodeMap.get(transform.getStartNodeId()));
            transformInfo.setEndStatusId(nodeMap.get(transform.getEndNodeId()));
            String rank = nodeRankMap.get(transform.getEndNodeId());
            if (ObjectUtils.isEmpty(rank)) {
                hasRankNull = true;
            }
            transformInfo.setRank(rank);
            //获取转换的条件配置
            List<StateMachineConfigVO> conditionConfigs = configMaps.get(transform.getId());
            if (conditionConfigs == null) {
                transformInfo.setConditions(Collections.emptyList());
            } else {
                transformInfo.setConditions(conditionConfigs);
                isNeedFilter = true;
            }
            transformInfos.add(transformInfo);
        }
        if (Boolean.FALSE.equals(hasRankNull)) {
            Collections.sort(transformInfos, (transformInfo1, transformInfo2) -> transformInfo1.getRank().compareTo(transformInfo2.getRank()));
        }
        //调用对应服务，根据条件校验转换，过滤掉不可用的转换
        if (Boolean.TRUE.equals(isNeedFilter)) {
            try {
                transformInfos = modelMapper.map(stateMachineClientService.conditionFilter(instanceId, modelMapper.map(transformInfos, new TypeToken<List<io.choerodon.agile.infra.statemachineclient.dto.TransformInfo>>() {
                }.getType())), new TypeToken<List<TransformInfo>>() {
                }.getType());
            } catch (Exception e) {
                LOGGER.error(EXCEPTION, e);
                transformInfos = Collections.emptyList();
            }
        }
        return transformInfos;
    }

    @Override
    public Boolean validatorGuard(Long organizationId, String serviceCode, Long transformId, InputDTO inputDTO, StateContext<String, String> context) {
        StatusMachineTransformDTO transform = transformMapper.queryById(organizationId, transformId);
        List<StateMachineConfigVO> conditionConfigs = condition(organizationId, transformId);
        List<StateMachineConfigVO> validatorConfigs = validator(organizationId, transformId);
        ExecuteResult executeResult = new ExecuteResult(true, null, null);
        //调用对应服务，执行条件和验证，返回是否成功
        try {
            if (!conditionConfigs.isEmpty()) {
                inputDTO.setConfigs(modelMapper.map(conditionConfigs, new TypeToken<List<StateMachineConfigVO>>() {
                }.getType()));
                executeResult = stateMachineClientService.configExecuteCondition(null, transform.getConditionStrategy(), modelMapper.map(inputDTO, InputDTO.class));
            }
            if (Boolean.TRUE.equals(executeResult.getSuccess()) && !validatorConfigs.isEmpty()) {
                inputDTO.setConfigs(modelMapper.map(validatorConfigs, new TypeToken<List<StateMachineConfigVO>>() {
                }.getType()));
                executeResult = stateMachineClientService.configExecuteValidator(null, modelMapper.map(inputDTO, InputDTO.class));
            }
        } catch (Exception e) {
            LOGGER.error(EXCEPTION, e);
            executeResult = new ExecuteResult(false, null, "验证调用失败");
        }

        Map<Object, Object> variables = context.getExtendedState().getVariables();
        variables.put("executeResult", executeResult);
        return executeResult.getSuccess();
    }

    @Override
    public Boolean postAction(Long organizationId, String serviceCode, Long transformId, InputDTO inputDTO, StateContext<String, String> context) {
        List<StateMachineConfigVO> configs = action(organizationId, transformId);
        inputDTO.setConfigs(modelMapper.map(configs, new TypeToken<List<StateMachineConfigVO>>() {
        }.getType()));
        StatusMachineTransformDTO transform = transformMapper.queryById(organizationId, transformId);
        //节点转状态
        Long targetStatusId = nodeDeployMapper.getNodeDeployById(Long.parseLong(context.getTarget().getId())).getStatusId();
        if (targetStatusId == null) {
            throw new CommonException("error.postAction.targetStatusId.notNull");
        }
        ExecuteResult executeResult;
        //调用对应服务，执行动作，返回是否成功
        try {
            executeResult = stateMachineClientService.configExecutePostAction(targetStatusId, transform.getType(), modelMapper.map(inputDTO, InputDTO.class));
            //返回为空则调用对应服务，对应服务方法报错
            if (executeResult == null) {
                executeResult = new ExecuteResult(false, null, "后置动作调用失败");
            }
        } catch (Exception e) {
            LOGGER.error(EXCEPTION, e);
            executeResult = new ExecuteResult(false, null, "后置动作调用失败");
        }
        Map<Object, Object> variables = context.getExtendedState().getVariables();
        variables.put("executeResult", executeResult);
        return executeResult.getSuccess();
    }

    @Override
    public List<StateMachineConfigVO> condition(Long organizationId, Long transformId) {
        List<StateMachineConfigVO> configs = configService.queryByTransformId(organizationId, transformId, ConfigType.CONDITION, false);
        return configs == null ? Collections.emptyList() : configs;
    }

    @Override
    public List<StateMachineConfigVO> validator(Long organizationId, Long transformId) {
        List<StateMachineConfigVO> configs = configService.queryByTransformId(organizationId, transformId, ConfigType.VALIDATOR, false);
        return configs == null ? Collections.emptyList() : configs;
    }

    @Override
    public List<StateMachineConfigVO> trigger(Long organizationId, Long transformId) {
        List<StateMachineConfigVO> configs = configService.queryByTransformId(organizationId, transformId, ConfigType.TRIGGER, false);
        return configs == null ? Collections.emptyList() : configs;
    }

    @Override
    public List<StateMachineConfigVO> action(Long organizationId, Long transformId) {
        List<StateMachineConfigVO> configs = configService.queryByTransformId(organizationId, transformId, ConfigType.ACTION, false);
        return configs == null ? Collections.emptyList() : configs;
    }

    @Override
    public Map<Long, Long> queryInitStatusIds(Long organizationId, List<Long> stateMachineIds) {
        if (!stateMachineIds.isEmpty()) {
            return nodeDeployMapper.queryInitByStateMachineIds(stateMachineIds, organizationId).stream()
                    .collect(Collectors.toMap(StatusMachineNodeDTO::getStateMachineId, StatusMachineNodeDTO::getStatusId));
        } else {
            return new HashMap<>();
        }
    }

    /**
     * 创建实例时，获取状态机的初始转换
     *
     * @param organizationId organizationId
     * @param stateMachineId stateMachineId
     * @return result
     */
    @Override
    public StatusMachineTransformVO queryInitTransform(Long organizationId, Long stateMachineId) {
        //获取初始转换
        StatusMachineTransformDTO initTransform = transformService.getInitTransform(organizationId, stateMachineId);
        StatusMachineTransformVO statusMachineTransformVO = modelMapper.map(initTransform, StatusMachineTransformVO.class);
        //获取转换配置
        List<StateMachineConfigVO> configDTOS = configService.queryByTransformId(organizationId, initTransform.getId(), null, false);
        Map<String, List<StateMachineConfigVO>> configMap = configDTOS.stream().collect(Collectors.groupingBy(StateMachineConfigVO::getType));
        statusMachineTransformVO.setConditions(configMap.get(ConfigType.CONDITION));
        statusMachineTransformVO.setTriggers(configMap.get(ConfigType.TRIGGER));
        statusMachineTransformVO.setValidators(configMap.get(ConfigType.VALIDATOR));
        statusMachineTransformVO.setPostpositions(configMap.get(ConfigType.ACTION));
        return statusMachineTransformVO;
    }
}
