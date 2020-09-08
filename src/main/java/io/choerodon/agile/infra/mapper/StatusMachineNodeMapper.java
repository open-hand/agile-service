package io.choerodon.agile.infra.mapper;

import io.choerodon.agile.infra.dto.IssueCountDTO;
import io.choerodon.agile.infra.dto.StatusMachineNodeDTO;
import io.choerodon.mybatis.common.BaseMapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

/**
 * @author peng.jiang, dinghuang123@gmail.com
 */
public interface StatusMachineNodeMapper extends BaseMapper<StatusMachineNodeDTO> {

    StatusMachineNodeDTO getNodeDeployById(@Param("nodeId") Long nodeId);

    StatusMachineNodeDTO getNodeDeployByStatusId(@Param("stateMachineId") Long stateMachineId, @Param("statusId") Long statusId);


    List<StatusMachineNodeDTO> selectByStateMachineId(@Param("stateMachineId") Long stateMachineId);

    Long checkStateDelete(@Param("organizationId") Long organizationId, @Param("statusId") Long statusId);

    StatusMachineNodeDTO queryById(@Param("organizationId") Long organizationId, @Param("id") Long id);

    List<StatusMachineNodeDTO> queryInitByStateMachineIds(@Param("stateMachineIds") List<Long> stateMachineIds, @Param("organizationId") Long organizationId);

    /**
     * 获取最大的postionY
     *
     * @param stateMachineId
     * @return
     */
    StatusMachineNodeDTO selectMaxPositionY(@Param("stateMachineId") Long stateMachineId);

    /**
     * 单独写更新，版本号不变，否则前端处理复杂
     */
    int updateAllStatusTransformId(@Param("organizationId") Long organizationId, @Param("id") Long id, @Param("allStatusTransformId") Long allStatusTransformId);

    List<StatusMachineNodeDTO> queryByStateMachineIds(@Param("organizationId") Long organizationId, @Param("stateMachineIds") List<Long> stateMachineIds);

    /**
     * 批量插入node
     * @param nodeList
     * @return
     */
    int batchInsert(@Param("list") List<StatusMachineNodeDTO> nodeList);

    List<IssueCountDTO> countIssueTypeByStatusIds(@Param("organizationId") Long organizationId,@Param("schemeId") Long schemeId, @Param("statusIds") List<Long> statusIds,@Param("applyType") String applyType);

    List<StatusMachineNodeDTO> selectInitNode(@Param("organizationId")  Long organizationId, @Param("schemeId") Long schemeId,@Param("statusId") Long statusId);

    void migrateStatusMachineNode();

    boolean existByProjectId(@Param("projectId") Long projectId, @Param("statusId") Long statusId);
}
