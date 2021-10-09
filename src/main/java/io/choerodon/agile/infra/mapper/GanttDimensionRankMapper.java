package io.choerodon.agile.infra.mapper;

import io.choerodon.agile.infra.dto.GanttDimensionRankDTO;
import io.choerodon.mybatis.common.BaseMapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;
import java.util.Set;

/**
 * @author superlee
 * @since 2021-10-09
 */
public interface GanttDimensionRankMapper extends BaseMapper<GanttDimensionRankDTO> {

    List<GanttDimensionRankDTO> orderByInstanceId(@Param("instanceIds") Set<Long> instanceIds,
                                                  @Param("projectId") Long projectId,
                                                  @Param("dimension") String dimension);

    void batchInsert(@Param("ganttDimensionRanks") List<GanttDimensionRankDTO> ganttDimensionRanks);

    String selectMaxPreviousRankOrderByRankAsc(@Param("organizationId") Long organizationId,
                                               @Param("projectId") Long projectId,
                                               @Param("dimension") String dimension,
                                               @Param("rank") String rank);
}
