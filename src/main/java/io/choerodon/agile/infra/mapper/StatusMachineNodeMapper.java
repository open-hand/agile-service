package io.choerodon.agile.infra.mapper;

import io.choerodon.agile.api.vo.StatusMachineSchemeConfigVO;
import io.choerodon.agile.infra.dto.IssueCountDTO;
import io.choerodon.agile.infra.dto.StatusMachineNodeDTO;
import io.choerodon.mybatis.common.BaseMapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;
import java.util.Set;

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

    List<IssueCountDTO> countStatusIssueTypeScope(@Param("organizationId") Long organizationId,
                                                  @Param("schemeIds") List<Long> schemeIds,
                                                  @Param("statusIds") List<Long> statusIds,
                                                  @Param("issueTypeCodes") List<String> issueTypeCodes);

    List<StatusMachineSchemeConfigVO> selectInitNode(@Param("organizationId")  Long organizationId, @Param("projectId")  Long projectId, @Param("applyTypes") List<String> applyTypes, @Param("statusId") Long statusId);

    void migrateStatusMachineNode();

    boolean existByProjectId(@Param("projectId") Long projectId, @Param("statusId") Long statusId,@Param("applyType") String applyType);

    List<StatusMachineNodeDTO> selectNullRankNodes(@Param("organizationId") Long organizationId, @Param("statusMachineId") Long statusMachineId);

    String queryLeftRank(@Param("organizationId") Long organizationId, @Param("statusMachineId") Long statusMachineId, @Param("rank") String rank);

    String queryRightRank(@Param("organizationId") Long organizationId, @Param("statusMachineId") Long statusMachineId, @Param("rank") String rank);

    String queryMaxRank(@Param("organizationId") Long organizationId, @Param("statusMachineId") Long statusMachineId);

    String queryMinRank(@Param("organizationId") Long organizationId, @Param("statusMachineId") Long statusMachineId);

    List<Long> selectStatusIdsByIssueTypeIds(@Param("organizationId") Long organizationId,
                                             @Param("schemeId") Long schemeId,
                                             @Param("issueTypeIds") Set<Long> issueTypeIds,
                                             @Param("applyType") String applyType);
}
