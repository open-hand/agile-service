package io.choerodon.agile.infra.mapper;

import io.choerodon.agile.api.vo.WorkHoursLogVO;
import io.choerodon.agile.api.vo.WorkHoursSearchVO;
import io.choerodon.agile.api.vo.business.IssueVO;
import org.apache.ibatis.annotations.Param;

import java.util.List;
import java.util.Set;

/**
 * @author zhaotianxin
 * @date 2021-10-18 10:16
 */
public interface WorkHoursMapper {

    List<WorkHoursLogVO> listByProjectIds(@Param("projectIds") List<Long> projectIds, @Param("workHoursSearchVO") WorkHoursSearchVO workHoursSearchVO);

    List<WorkHoursLogVO> listGroupDataByProjectIds(@Param("projectIds") List<Long> projectIds, @Param("workHoursSearchVO") WorkHoursSearchVO workHoursSearchVO);

    List<WorkHoursLogVO> countUserWorkTime(@Param("projectIds") List<Long> projectIds, @Param("workHoursSearchVO") WorkHoursSearchVO workHoursSearchVO);

    Set<Long> selectUserIds(@Param("projectIds") List<Long> projectIds, @Param("workHoursSearchVO") WorkHoursSearchVO workHoursSearchVO);

    List<IssueVO> queryIssue(@Param("projectId") Long projectId, @Param("params") String params);
}
