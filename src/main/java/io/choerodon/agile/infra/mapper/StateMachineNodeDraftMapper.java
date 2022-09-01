package io.choerodon.agile.infra.mapper;

import java.util.List;

import org.apache.ibatis.annotations.Param;

import io.choerodon.agile.infra.dto.StateMachineNodeDraftDTO;
import io.choerodon.mybatis.common.BaseMapper;

/**
 * @author peng.jiang, dinghuang123@gmail.com
 */
public interface StateMachineNodeDraftMapper extends BaseMapper<StateMachineNodeDraftDTO> {

    StateMachineNodeDraftDTO getNodeById(@Param("nodeId") Long nodeId);

    List<StateMachineNodeDraftDTO> selectByStateMachineId(@Param("stateMachineId") Long stateMachineId);

    Long checkStateDelete(@Param("organizationId") Long organizationId, @Param("statusId") Long statusId);

    StateMachineNodeDraftDTO queryById(@Param("organizationId") Long organizationId, @Param("id") Long id);

    /**
     * 获取最大的postionY
     *
     * @param stateMachineId stateMachineId
     * @return result
     */
    StateMachineNodeDraftDTO selectMaxPositionY(@Param("stateMachineId") Long stateMachineId);

    /**
     * 单独写更新，版本号不变，否则前端处理复杂
     */
    int updateAllStatusTransformId(@Param("organizationId") Long organizationId, @Param("id") Long id, @Param("allStatusTransformId") Long allStatusTransformId);
}
