package io.choerodon.agile.app.service;

import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.infra.dto.business.IssueDTO;
import io.choerodon.core.domain.Page;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;

import java.util.List;
import java.util.Map;

/**
 * @author superlee
 * @since 2020-11-24
 */
public interface GanttChartService {

    /**
     * 查询甘特图列表数据(任务视图)
     *
     * @param projectId
     * @param searchVO
     * @return
     */
    Page<GanttChartVO> pagedQuery(Long projectId, SearchVO searchVO, PageRequest pageRequest);

    List<GanttChartVO> listByIds(Long projectId, GanttChartSearchVO ganttChartSearchVO, String dimension);

    void move(Long projectId, GanttMoveVO ganttMoveVO);

    GanttDimensionListVO ganttDimensionList(Long projectId, SearchVO searchVO);

    void moveDimension(Long projectId, GanttDimensionMoveVO ganttDimensionMoveVO);

    Page<GanttChartVO> listByProjectIdAndSearch(Map<Long, ProjectVO> projectMap,
                                                SearchVO searchVO,
                                                PageRequest pageRequest,
                                                Long organizationId,
                                                boolean orderByRank);

    String getFilterSql(SearchVO searchVO);

    void processSort(PageRequest pageRequest, Map<String, Object> sortMap);

    List<GanttChartVO> buildGanttList(Map<Long, ProjectVO> projectMap,
                                      List<Long> issueIds,
                                      List<IssueDTO> issueList,
                                      Map<Long, Long> issueEpicMap,
                                      Map<Long, IssueDTO> issueFeatureMap,
                                      List<ObjectSchemeFieldVO> displayFields,
                                      Long organizationId);

    void saveSort(Long projectId, List<IssuePersonalSortVO> issuePersonalSorts);

    List<IssuePersonalSortVO> listLatestSort(Long projectId);
}
