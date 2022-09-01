package io.choerodon.agile.app.service;

import java.util.List;
import java.util.Map;

import io.choerodon.agile.api.vo.NodeSortVO;
import io.choerodon.agile.api.vo.StatusMachineNodeVO;
import io.choerodon.agile.api.vo.StatusVO;
import io.choerodon.agile.infra.dto.StatusMachineNodeDTO;

/**
 * @author peng.jiang, dinghuang123@gmail.com
 */
public interface StateMachineNodeService {
    /**
     * 创建状态节点
     *
     * @param organizationId organizationId
     * @param nodeVO nodeVO
     * @return result
     */
    List<StatusMachineNodeVO> create(Long organizationId, Long stateMachineId, StatusMachineNodeVO nodeVO);

    /**
     * 更新节点
     *
     * @param organizationId 组织id
     * @param nodeId         节点id
     * @param nodeVO        节点对象
     * @return 更新节点
     */
    List<StatusMachineNodeVO> update(Long organizationId, Long stateMachineId, Long nodeId, StatusMachineNodeVO nodeVO);

    /**
     * 删除状态节点
     *
     * @param organizationId 组织id
     * @param nodeId         节点id
     * @return result
     */
    List<StatusMachineNodeVO> delete(Long organizationId, Long stateMachineId, Long nodeId);

    /**
     * 校验是否能删除状态节点
     *
     * @param organizationId organizationId
     * @param statusId statusId
     * @return result
     */
    Map<String, Object> checkDelete(Long organizationId, Long stateMachineId, Long statusId);

    /**
     * 获取状态机初始节点id
     *
     * @param stateMachineId stateMachineId
     * @return result
     */
    Long getInitNode(Long organizationId, Long stateMachineId);

    /**
     * 根据id获取节点
     *
     * @param organizationId organizationId
     * @param nodeId nodeId
     * @return result
     */
    StatusMachineNodeVO queryById(Long organizationId, Long nodeId);

    /**
     * 根据状态机id获取所有节点
     *
     * @param organizationId organizationId
     * @param stateMachineId stateMachineId
     * @return result
     */
    List<StatusMachineNodeVO> queryByStateMachineId(Long organizationId, Long stateMachineId, Boolean isDraft);

    /**
     * 敏捷创建节点
     *
     * @param organizationId organizationId
     * @param stateMachineId stateMachineId
     * @param statusVO statusVO
     */
    void createNodeAndTransformForAgile(Long organizationId, Long stateMachineId, StatusVO statusVO);

    void baseUpdate(StatusMachineNodeDTO olderDefaultNode);

    void baseCreate(StatusMachineNodeDTO statusMachineNodeDTO);

    void handlerNullRankNode(Long organizationId, Long statusMachineId, String applyType);

    void sortNode(Long organizationId, Long statusMachineId, NodeSortVO nodeSortVO, String applyType);
}
