package io.choerodon.agile.infra.mapper;

import io.choerodon.agile.api.vo.IssueWorkHoursVO;
import io.choerodon.agile.api.vo.SearchVO;
import io.choerodon.agile.api.vo.WorkHoursLogVO;
import io.choerodon.agile.api.vo.WorkHoursSearchVO;
import io.choerodon.agile.api.vo.business.IssueVO;
import io.choerodon.agile.infra.dto.WorkLogDTO;
import io.choerodon.agile.infra.dto.business.IssueDTO;
import org.apache.ibatis.annotations.Param;

import java.util.List;
import java.util.Map;
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

    List<IssueDTO> queryParentIssueIdsList(@Param("projectIds") Set<Long> projectIds,
                                           @Param("searchVO") SearchVO searchVO,
                                           @Param("filterSql") String filterSql,
                                           @Param("assigneeFilterIds") List<Long> assigneeFilterIds,
                                           @Param("sortMap") Map<String, Object> sortMap);

    List<WorkLogDTO> countWorkTime(@Param("projectIds") List<Long> projectIds, @Param("allIssueIds") List<Long> allIssueIds, @Param("searchVO") SearchVO searchVO);

    List<IssueWorkHoursVO> countAssigneeWorkTime(@Param("projectIds") List<Long> projectIds, @Param("allIssueIds") List<Long> allIssueIds, @Param("searchVO") SearchVO searchVO);

    List<IssueWorkHoursVO> countProjectAssigneeWorkTime(@Param("projectIds") List<Long> projectIds, @Param("allIssueIds") List<Long> allIssueIds, @Param("searchVO") SearchVO searchVO);

    List<IssueWorkHoursVO> countProjectWorkTime(@Param("projectIds") List<Long> projectIds, @Param("allIssueIds") List<Long> allIssueIds, @Param("searchVO") SearchVO searchVO);

    List<IssueWorkHoursVO> countProjectEstimateTime(@Param("projectIds") List<Long> projectIds, @Param("allIssueIds") List<Long> allIssueIds);

    List<IssueWorkHoursVO> countAssigneeEstimateTime(@Param("projectIds") List<Long> projectIds, @Param("allIssueIds") List<Long> allIssueIds);

    List<Long> queryIds(@Param("projectIds") List<Long> projectIds, @Param("issueIds") List<Long> issueIds, @Param("levelType") String levelType);

    List<IssueWorkHoursVO> queryProjectAssigneeIds(@Param("projectIds") List<Long> projects, @Param("issueIds") List<Long> issueIds);

    List<IssueDTO> queryChildrenIdByParentId(@Param("issueIds") List<Long> issueIds,
                                             @Param("projectIds") Set<Long> projectIds,
                                             @Param("searchVO") SearchVO searchVO,
                                             @Param("filterSql") String filterSql,
                                             @Param("assigneeFilterIds") List<Long> assigneeFilterIds,
                                             @Param("sortMap") Map<String, Object> sortMap);
}
