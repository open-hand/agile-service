package io.choerodon.agile.app.service;

import java.util.List;
import java.util.Map;

import org.springframework.statemachine.StateContext;

import io.choerodon.agile.api.vo.StateMachineConfigVO;
import io.choerodon.agile.api.vo.StatusMachineTransformVO;
import io.choerodon.agile.api.vo.event.TransformInfo;
import io.choerodon.agile.infra.statemachineclient.dto.ExecuteResult;
import io.choerodon.agile.infra.statemachineclient.dto.InputDTO;

/**
 * @author shinan.chen
 * @date 2018/9/18
 */
public interface InstanceService {

    /**
     * 创建状态机实例，并返回初始状态
     *
     * @param serviceCode serviceCode
     * @param stateMachineId stateMachineId
     * @return result
     */
    ExecuteResult startInstance(Long organizationId, String serviceCode, Long stateMachineId, InputDTO inputDTO);

    /**
     * 查询状态机的初始状态id
     *
     * @param organizationId organizationId
     * @param stateMachineId stateMachineId
     * @return result
     */
    Long queryInitStatusId(Long organizationId, Long stateMachineId);

    /**
     * 执行状态转换，并返回转换后的状态
     *
     * @param stateMachineId  状态机Id
     * @param transformId     转换Id
     * @param currentStatusId 当前状态Id
     * @param serviceCode     请求服务code
     * @return result
     */
    ExecuteResult executeTransform(Long organizationId, String serviceCode, Long stateMachineId, Long currentStatusId, Long transformId, InputDTO inputDTO);

    /**
     * 获取当前状态拥有的转换列表，feign调用对应服务的条件验证
     *
     * @param organizationId organizationId
     * @param statusId statusId
     * @return result
     */
    List<TransformInfo> queryListTransform(Long organizationId, String serviceCode, Long stateMachineId, Long instanceId, Long statusId);

    /**
     * 调用相应服务，验证转换
     *
     * @param organizationId organizationId
     * @param serviceCode serviceCode
     * @param transformId transformId
     * @param inputDTO inputDTO
     * @param context        状态机上下文，传递参数
     * @return result
     */
    Boolean validatorGuard(Long organizationId, String serviceCode, Long transformId, InputDTO inputDTO, StateContext<String, String> context);

    /**
     * 调用相应服务，执行后置动作
     *
     * @param organizationId organizationId
     * @param serviceCode serviceCode
     * @param transformId transformId
     * @param inputDTO inputDTO
     * @param context        状态机上下文，传递参数
     * @return result
     */
    Boolean postAction(Long organizationId, String serviceCode, Long transformId, InputDTO inputDTO, StateContext<String, String> context);

    /**
     * 条件
     *
     * @param transformId 转换id
     * @return result
     */
    List<StateMachineConfigVO> condition(Long organizationId, Long transformId);

    /**
     * 验证器
     *
     * @param transformId 转换id
     * @return result
     */
    List<StateMachineConfigVO> validator(Long organizationId, Long transformId);

    /**
     * 触发器
     *
     * @param transformId 转换id
     * @return result
     */
    List<StateMachineConfigVO> trigger(Long organizationId, Long transformId);

    /**
     * 后置功能
     *
     * @param transformId 转换id
     * @return result
     */
    List<StateMachineConfigVO> action(Long organizationId, Long transformId);

    /**
     * 获取状态机列表对应的状态机初始状态map
     *
     * @param organizationId  organizationId
     * @param stateMachineIds stateMachineIds
     * @return Map
     */
    Map<Long, Long> queryInitStatusIds(Long organizationId, List<Long> stateMachineIds);

    /**
     * 创建实例时，获取状态机的初始转换
     *
     * @param organizationId organizationId
     * @param stateMachineId stateMachineId
     * @return result
     */
    StatusMachineTransformVO queryInitTransform(Long organizationId, Long stateMachineId);
}
