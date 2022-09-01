package io.choerodon.agile.app.service;

import java.util.List;
import java.util.Map;

import io.choerodon.agile.api.vo.StateMachineTransformUpdateVO;
import io.choerodon.agile.api.vo.StatusMachineTransformVO;
import io.choerodon.agile.api.vo.TransformVO;
import io.choerodon.agile.infra.dto.StatusMachineTransformDTO;

/**
 * @author peng.jiang, dinghuang123@gmail.com
 */
public interface StateMachineTransformService {

    /**
     * 创建转换
     *
     * @param organizationId organizationId
     * @param transformVO transformVO
     * @return result
     */
    StatusMachineTransformVO create(Long organizationId, Long stateMachineId, StatusMachineTransformVO transformVO);

    /**
     * 更新转换
     *
     * @param organizationId 组织id
     * @param transformId    转换id
     * @param transformVO   转换对象
     * @return 更新转换
     */
    StatusMachineTransformVO update(Long organizationId, Long stateMachineId, Long transformId, StatusMachineTransformVO transformVO);

    /**
     * 删除转换
     *
     * @param organizationId 组织id
     * @param transformId    节点id
     * @return result
     */
    Boolean delete(Long organizationId, Long stateMachineId, Long transformId);

    /**
     * 获取初始转换
     *
     * @param stateMachineId stateMachineId
     * @return result
     */
    StatusMachineTransformDTO getInitTransform(Long organizationId, Long stateMachineId);

    /**
     * 根据id获取转换
     *
     * @param organizationId organizationId
     * @param transformId transformId
     * @return result
     */
    StatusMachineTransformVO queryById(Long organizationId, Long transformId);

    /**
     * 获取当前状态拥有的转换列表，包括【全部】类型的转换
     *
     * @param organizationId organizationId
     * @param stateMachineId stateMachineId
     * @param statusId statusId
     * @return result
     */
    List<StatusMachineTransformDTO> queryListByStatusIdByDeploy(Long organizationId, Long stateMachineId, Long statusId);

    /**
     * 创建【全部转换到此状态】转换，所有节点均可转换到当前节点
     *
     * @param organizationId 组织id
     * @param endNodeId endNodeId
     * @return result
     */
    StatusMachineTransformVO createAllStatusTransform(Long organizationId, Long stateMachineId, Long endNodeId);

    /**
     * 删除【全部转换到此状态】转换
     *
     * @param organizationId organizationId
     * @param transformId transformId
     * @return result
     */
    Boolean deleteAllStatusTransform(Long organizationId, Long transformId);

    /**
     * 更新转换的条件策略
     *
     * @param organizationId organizationId
     * @param transformId transformId
     * @param conditionStrategy conditionStrategy
     * @return result
     */
    Boolean updateConditionStrategy(Long organizationId, Long transformId, String conditionStrategy);

    /**
     * 校验名字是否重复
     *
     * @param organizationId organizationId
     * @param stateMachineId stateMachineId
     * @param name name
     * @return result
     */
    Boolean checkName(Long organizationId, Long stateMachineId, Long startNodeId, Long endNodeId, String name);

    /**
     * 根据状态机id列表查询出这些状态机每个状态对应的转换列表
     *
     * @param organizationId organizationId
     * @param stateMachineIds stateMachineIds
     * @return result
     */
    Map<Long, Map<Long, List<TransformVO>>> queryStatusTransformsMap(Long organizationId, List<Long> stateMachineIds);

    /**
     * 敏捷获取转换
     *
     * @param organizationId organizationId
     * @param transformId transformId
     * @return result
     */
    StatusMachineTransformDTO queryDeployTransformForAgile(Long organizationId, Long transformId);

    void createTransform(Long organizationId, Long stateMachineId, StateMachineTransformUpdateVO transformUpdateVO);

    void deleteTransformByNodeId(Long organizationId, Long stateMachineId, Long startNodeId, Long endNodeId);
}
