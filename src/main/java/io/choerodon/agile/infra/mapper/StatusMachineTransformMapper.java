package io.choerodon.agile.infra.mapper;

import java.util.List;

import org.apache.ibatis.annotations.Param;

import io.choerodon.agile.infra.dto.StatusMachineTransformDTO;
import io.choerodon.mybatis.common.BaseMapper;

/**
 * @author peng.jiang, dinghuang123@gmail.com
 */
public interface StatusMachineTransformMapper extends BaseMapper<StatusMachineTransformDTO> {
    /**
     * 删除节点时，删除关联的转换
     *
     * @param nodeId 节点id
     * @return result
     */
    int deleteByNodeId(Long nodeId);

    StatusMachineTransformDTO queryById(@Param("organizationId") Long organizationId, @Param("id") Long id);

    /**
     * 获取某个节点拥有的转换（包括全部转换）
     *
     * @param organizationId organizationId
     * @param stateMachineId stateMachineId
     * @param startNodeId startNodeId
     * @param transformType transformType
     * @return result
     */
    List<StatusMachineTransformDTO> queryByStartNodeIdOrType(@Param("organizationId") Long organizationId, @Param("stateMachineId") Long stateMachineId, @Param("startNodeId") Long startNodeId, @Param("transformType") String transformType);

    List<StatusMachineTransformDTO> queryByStateMachineIds(@Param("organizationId") Long organizationId, @Param("stateMachineIds") List<Long> stateMachineIds);

    void batchInsert(@Param("list") List<StatusMachineTransformDTO> newStateMachineTransformDTO);

    int deleteByStateMachineIdAndNodeId(@Param("organizationId") Long organizationId, @Param("stateMachineId") Long stateMachineId,@Param("nodeId") Long nodeId);

    void migrateStatusMachineTransform();

    List<StatusMachineTransformDTO> selectTransformByStatusId(@Param("organizationId") Long organizationId,
                                                              @Param("stateMachineId") Long stateMachineId,
                                                              @Param("statusId") Long statusId,
                                                              @Param("changeStatus") Long changeStatus,
                                                              @Param("transformAllFlag") boolean transformAllFlag);

    List<Long> existTransferOwner(@Param("stateMachineId") Long stateMachineId);
}
