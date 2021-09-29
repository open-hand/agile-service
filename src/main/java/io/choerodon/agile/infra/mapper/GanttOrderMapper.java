package io.choerodon.agile.infra.mapper;

import io.choerodon.agile.infra.dto.GanttOrderDTO;
import io.choerodon.mybatis.common.BaseMapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;
import java.util.Set;

/**
 * @author superlee
 * @since 2021-09-28
 */
public interface GanttOrderMapper extends BaseMapper<GanttOrderDTO> {

    List<GanttOrderDTO> selectByOptions(@Param("organizationId") Long organizationId,
                                        @Param("projectId") Long projectId,
                                        @Param("instanceId") Long instanceId,
                                        @Param("instanceType") String instanceType,
                                        @Param("dimension") String dimension,
                                        @Param("issueIds") Set<Long> issueIds);


    void batchInsert(@Param("ganttOrders") List<GanttOrderDTO> ganttOrders);

    GanttOrderDTO selectMinRank(@Param("organizationId") Long organizationId,
                                @Param("projectId") Long projectId,
                                @Param("instanceId") Long instanceId,
                                @Param("instanceType") String instanceType,
                                @Param("dimension") String dimension);

    List<GanttOrderDTO> selectByIssueIdWithRank(@Param("organizationId") Long organizationId,
                                                @Param("projectId") Long projectId,
                                                @Param("instanceId") Long instanceId,
                                                @Param("instanceType") String instanceType,
                                                @Param("dimension") String dimension,
                                                @Param("issueIds") List<Long> issueIds);

    String selectMaxPreviousRankOrderByRankAsc(@Param("organizationId") Long organizationId,
                                               @Param("projectId") Long projectId,
                                               @Param("instanceId") Long instanceId,
                                               @Param("instanceType") String instanceType,
                                               @Param("dimension") String dimension,
                                               @Param("rank") String rank);
}
