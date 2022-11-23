package io.choerodon.agile.infra.factory;

import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.statemachine.StateMachine;
import org.springframework.statemachine.action.Action;
import org.springframework.statemachine.config.StateMachineBuilder;
import org.springframework.statemachine.guard.Guard;
import org.springframework.statemachine.state.State;
import org.springframework.statemachine.support.DefaultStateMachineContext;
import org.springframework.stereotype.Component;

import io.choerodon.agile.app.service.*;
import io.choerodon.agile.infra.cache.InstanceCache;
import io.choerodon.agile.infra.dto.StatusMachineDTO;
import io.choerodon.agile.infra.dto.StatusMachineNodeDTO;
import io.choerodon.agile.infra.dto.StatusMachineTransformDTO;
import io.choerodon.agile.infra.enums.TransformType;
import io.choerodon.agile.infra.mapper.StatusMachineNodeMapper;
import io.choerodon.agile.infra.statemachineclient.dto.ExecuteResult;
import io.choerodon.agile.infra.statemachineclient.dto.InputDTO;
import io.choerodon.core.exception.CommonException;

/**
 * @author shinan.chen
 * @date 2018/9/14
 */
@Component
public class MachineFactory {
    private static Logger logger = LoggerFactory.getLogger(MachineFactory.class);

    private static final String EXECUTE_RESULT = "executeResult";
    private static final String INPUT_DTO = "InputDTO";
    @Autowired
    private StateMachineClientService stateMachineClientService;
    @Autowired
    private StateMachineService stateMachineService;
    @Autowired
    private StateMachineTransformService transformService;
    @Autowired
    private StateMachineNodeService nodeService;
    @Autowired
    private StatusMachineNodeMapper nodeDeployMapper;
    @Autowired
    private InstanceService instanceService;
    @Autowired
    private InstanceCache instanceCache;

    private StateMachineBuilder.Builder<String, String> getBuilder(Long organizationId, String serviceCode, Long stateMachineId) {
        StatusMachineDTO stateMachine = stateMachineService.queryDeployForInstance(organizationId, stateMachineId);
        List<StatusMachineNodeDTO> nodes = stateMachine.getNodes();
        List<StatusMachineTransformDTO> transforms = stateMachine.getTransforms();
        Long initNodeId = nodeService.getInitNode(organizationId, stateMachineId);

        StateMachineBuilder.Builder<String, String> builder = StateMachineBuilder.builder();
        try {
            builder.configureConfiguration()
                    .withConfiguration()
                    .machineId(stateMachineId.toString());
            builder.configureStates()
                    .withStates()
                    .initial(initNodeId.toString(), initialAction(organizationId, serviceCode))
                    .states(nodes.stream().map(x -> x.getId().toString()).collect(Collectors.toSet()));
            for (StatusMachineTransformDTO transform : transforms) {
                if (transform.getType().equals(TransformType.ALL)) {
                    //若配置了全部转换
                    for (StatusMachineNodeDTO node : nodes) {
                        String event = transform.getId().toString();
                        String source = node.getId().toString();
                        String target = transform.getEndNodeId().toString();
                        builder.configureTransitions()
                                .withExternal()
                                .source(source).target(target)
                                .event(event)
                                .action(action(organizationId, serviceCode), errorAction(organizationId, serviceCode))
                                .guard(guard(organizationId, serviceCode));
                    }
                } else {
                    //转换都是通过id配置
                    String event = transform.getId().toString();
                    String source = transform.getStartNodeId().toString();
                    String target = transform.getEndNodeId().toString();
                    builder.configureTransitions()
                            .withExternal()
                            .source(source).target(target)
                            .event(event)
                            .action(action(organizationId, serviceCode), errorAction(organizationId, serviceCode))
                            .guard(guard(organizationId, serviceCode));
                }

            }
        } catch (Exception e) {
            logger.error("build StateMachineBuilder error,exception:{},stateMachineId:{}", e, stateMachineId);
        }
        return builder;
    }

    private StateMachine<String, String> buildInstance(Long organizationId, String serviceCode, Long stateMachineId) {
        StateMachineBuilder.Builder<String, String> builder = instanceCache.getBuilder(stateMachineId);
        if (builder == null) {
            builder = getBuilder(organizationId, serviceCode, stateMachineId);
            logger.info("build StateMachineBuilder successful,stateMachineId:{}", stateMachineId);
            instanceCache.putBuilder(stateMachineId, builder);
        }
        StateMachine<String, String> smInstance = builder.build();
        smInstance.start();
        return smInstance;
    }

    /**
     * 开始实例
     *
     * @param serviceCode serviceCode
     * @param stateMachineId stateMachineId
     * @return result
     */
    public ExecuteResult startInstance(Long organizationId, String serviceCode, Long stateMachineId, InputDTO inputDTO) {
        StateMachine<String, String> instance = buildInstance(organizationId, serviceCode, stateMachineId);
        //存入instanceId，以便执行guard和action
        instance.getExtendedState().getVariables().put(INPUT_DTO, inputDTO);
        //执行初始转换
        Long initTransformId = transformService.getInitTransform(organizationId, stateMachineId).getId();
        instance.sendEvent(initTransformId.toString());

        //缓存实例
        instanceCache.putInstance(serviceCode, stateMachineId, inputDTO.getInstanceId(), instance);
        Object obj = instance.getExtendedState().getVariables().get(EXECUTE_RESULT);
        ExecuteResult executeResult = new ExecuteResult();
        if (obj != null) {
            executeResult = (ExecuteResult) obj;
        } else {
            executeResult.setSuccess(false);
            executeResult.setErrorMessage("触发事件失败");
        }
        return executeResult;
    }

    /**
     * 状态转换
     *
     * @param serviceCode serviceCode
     * @param stateMachineId stateMachineId
     * @param currentStatusId currentStatusId
     * @param transformId transformId
     * @return result
     */
    public ExecuteResult executeTransform(Long organizationId, String serviceCode, Long stateMachineId, Long currentStatusId, Long transformId, InputDTO inputDTO) {
        try {
            Long instanceId = inputDTO.getInstanceId();
            //校验transformId是否合法
            List<StatusMachineTransformDTO> transforms = transformService.queryListByStatusIdByDeploy(organizationId, stateMachineId, currentStatusId);
            StatusMachineTransformDTO transform = transforms.stream().filter(t -> Objects.equals(t.getId(), transformId)).findAny().orElse(null);
            if (transform == null) {
                throw new CommonException("error.executeTransform.transformId.illegal");
            }
            //状态转节点
            Long currentNodeId = nodeDeployMapper.getNodeDeployByStatusId(stateMachineId, currentStatusId).getId();
            StateMachine<String, String> instance =
                    getInstance(organizationId, serviceCode, stateMachineId, instanceId, currentNodeId);
            if (!isInstanceLatest(transform, instance)) {
                //ConcurrentHashMap为内存缓存，在分布式多pod环境中，存在pod中的缓存无法同步的问题。
                //例如添加一个状态，执行添加操作是在pod1中进行的，则其他pod的内存中没有这个状态，导致instance.sendEvent无法发送事件。
                //这里判断下pod里的instance是否为最新的，如果不是，则重新构建一次
                logger.info("the instance of this pod is not latest, so clean the cache and rebuild instance.");
                instanceCache.cleanStateMachine(stateMachineId);
                instance =
                        getInstance(organizationId, serviceCode, stateMachineId, instanceId, currentNodeId);
            }
            //存入instanceId，以便执行guard和action
            instance.getExtendedState().getVariables().put(INPUT_DTO, inputDTO);
            //触发事件
            instance.sendEvent(transformId.toString());

            //节点转状态
            Long statusId = nodeDeployMapper.getNodeDeployById(Long.parseLong(instance.getState().getId())).getStatusId();
            Object executeResult = instance.getExtendedState().getVariables().get(EXECUTE_RESULT);
            if (executeResult == null) {
                logger.error("send event failed, the result of instance is null");
                executeResult = new ExecuteResult();
                ((ExecuteResult) executeResult).setSuccess(false);
                ((ExecuteResult) executeResult).setResultStatusId(statusId);
                ((ExecuteResult) executeResult).setErrorMessage("触发事件失败");
            }
            return (ExecuteResult) executeResult;
        } catch (Exception e) {
            ExecuteResult executeResult = new ExecuteResult();
            executeResult.setException(e);
            executeResult.setSuccess(false);
            executeResult.setResultStatusId(null);
            executeResult.setErrorMessage("执行转换失败");
            return executeResult;
        }

    }

    private boolean isInstanceLatest(StatusMachineTransformDTO transform, StateMachine<String, String> instance) {
        boolean flag = false;
        // 检查状态机配置是否改变
        for (State<String, String> s :instance.getStates()) {
            if (Long.valueOf(s.getId()).equals(transform.getEndNodeId())) {
                flag = true;
                break;
            }
        }
        // 检查状态机内当前状态是否与传入状态相同
        return flag && Long.valueOf(instance.getState().getId()).equals(transform.getStartNodeId());
    }

    private StateMachine<String, String> getInstance(Long organizationId, String serviceCode, Long stateMachineId, Long instanceId, Long currentNodeId) {
        StateMachine<String, String> instance = instanceCache.getInstance(serviceCode, stateMachineId, instanceId);
        if (instance == null) {
            instance = buildInstance(organizationId, serviceCode, stateMachineId);
            //恢复节点
            String id = instance.getId();
            instance.getStateMachineAccessor()
                    .doWithAllRegions(access ->
                            access.resetStateMachine(new DefaultStateMachineContext<>(currentNodeId.toString(), null, null, null, null, id)));
            logger.info("restore stateMachine instance successful, stateMachineId:{}", stateMachineId);
            instanceCache.putInstance(serviceCode, stateMachineId, instanceId, instance);
        }
        return instance;
    }

    /**
     * 初始化动作
     *
     * @param serviceCode serviceCode
     * @return result
     */
    private Action<String, String> initialAction(Long organizationId, String serviceCode) {
        return context ->
                logger.info("stateMachine instance execute initialAction,organizationId:{},serviceCode:{}", organizationId, serviceCode);
//                instanceService.postAction()

    }

    /**
     * 转换动作
     *
     * @param serviceCode serviceCode
     * @return result
     */
    private Action<String, String> action(Long organizationId, String serviceCode) {
        return context -> {
            Long transformId = Long.parseLong(context.getEvent());
            InputDTO inputDTO = (InputDTO) context.getExtendedState().getVariables().get(INPUT_DTO);
            logger.info("stateMachine instance execute transform action,instanceId:{},transformId:{}", inputDTO.getInstanceId(), transformId);
            Boolean result = instanceService.postAction(organizationId, serviceCode, transformId, inputDTO, context);
            if (!result) {
                throw new CommonException("error.stateMachine.action");
            }
        };
    }

    /**
     * 转换出错动作
     *
     * @param serviceCode serviceCode
     * @return result
     */
    private Action<String, String> errorAction(Long organizationId, String serviceCode) {
        return context -> {
            Long transformId = Long.parseLong(context.getEvent());
            InputDTO inputDTO = (InputDTO) context.getExtendedState().getVariables().get(INPUT_DTO);
            logger.error("stateMachine instance execute transform error,organizationId:{},serviceCode:{},instanceId:{},transformId:{}", organizationId, serviceCode, inputDTO.getInstanceId(), transformId);
            // do something
        };
    }

    /**
     * 条件验证是否转换
     *
     * @param serviceCode serviceCode
     * @return result
     */
    private Guard<String, String> guard(Long organizationId, String serviceCode) {
        return context -> {
            Long transformId = Long.parseLong(context.getEvent());
            InputDTO inputDTO = (InputDTO) context.getExtendedState().getVariables().get(INPUT_DTO);
            logger.info("stateMachine instance execute transform guard,instanceId:{},transformId:{}", inputDTO.getInstanceId(), transformId);
            return instanceService.validatorGuard(organizationId, serviceCode, transformId, inputDTO, context);
        };
    }
}
