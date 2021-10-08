package io.choerodon.agile.infra.mapper;

import io.choerodon.agile.infra.dto.GanttIssueRankDTO;
import io.choerodon.mybatis.common.BaseMapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;
import java.util.Set;

/**
 * @author superlee
 * @since 2021-09-28
 */
public interface GanttIssueRankMapper extends BaseMapper<GanttIssueRankDTO> {

    List<GanttIssueRankDTO> selectByOptions(@Param("organizationId") Long organizationId,
                                            @Param("projectId") Long projectId,
                                            @Param("instanceId") Long instanceId,
                                            @Param("instanceType") String instanceType,
                                            @Param("dimension") String dimension,
                                            @Param("issueIds") Set<Long> issueIds);


    void batchInsert(@Param("ganttIssueRanks") List<GanttIssueRankDTO> ganttIssueRanks);

    GanttIssueRankDTO selectMinRank(@Param("organizationId") Long organizationId,
                                    @Param("projectId") Long projectId,
                                    @Param("instanceId") Long instanceId,
                                    @Param("instanceType") String instanceType,
                                    @Param("dimension") String dimension);

    List<GanttIssueRankDTO> selectByIssueIdWithRank(@Param("organizationId") Long organizationId,
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
