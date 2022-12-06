package io.choerodon.agile.app.service;

import java.util.List;
import java.util.Map;
import java.util.Set;

import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.api.vo.search.SearchParamVO;
import io.choerodon.agile.api.vo.waterfall.GanttParentInfoVO;
import io.choerodon.agile.infra.dto.business.IssueDTO;
import io.choerodon.core.domain.Page;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;

/**
 * @author superlee
 * @since 2020-11-24
 */
public interface GanttChartService {

    /**
     * @see GanttChartService#pagedQueryV2(Long, SearchParamVO, PageRequest)
     * 查询甘特图列表数据(任务视图)
     *
     * @param projectId projectId
     * @param searchVO searchVO
     * @return result
     */
    @Deprecated
    Page<GanttChartVO> pagedQuery(Long projectId, SearchVO searchVO, PageRequest pageRequest);

    Page<GanttChartVO> pagedQueryV2(Long projectId, SearchParamVO searchParamVO, PageRequest pageRequest);

    List<GanttChartVO> listByIds(Long projectId, GanttChartSearchVO ganttChartSearchVO, String dimension);

    /**
     * @see GanttChartService#moveV2(Long, GanttMoveVO)
     * @param projectId
     * @param ganttMoveVO
     */
    @Deprecated
    void move(Long projectId, GanttMoveVO ganttMoveVO);

    void moveV2(Long projectId, GanttMoveVO ganttMoveVO);

    /**
     * @see GanttChartService#ganttDimensionListV2(Long, SearchParamVO)
     * @param projectId
     * @param searchVO
     * @return
     */
    @Deprecated
    GanttDimensionListVO ganttDimensionList(Long projectId, SearchVO searchVO);

    GanttDimensionListVO ganttDimensionListV2(Long projectId, SearchParamVO searchParamVO);

    /**
     * @see GanttChartService#moveDimensionV2(Long, GanttDimensionMoveVO)
     * @param projectId
     * @param ganttDimensionMoveVO
     */
    @Deprecated
    void moveDimension(Long projectId, GanttDimensionMoveVO ganttDimensionMoveVO);

    void moveDimensionV2(Long projectId, GanttDimensionMoveVO ganttDimensionMoveVO);

    /**
     * @see GanttChartService#listByProjectIdAndSearchV2(Map, SearchParamVO, PageRequest, Long, boolean)
     * @param projectMap
     * @param searchVO
     * @param pageRequest
     * @param organizationId
     * @param orderByRank
     * @return
     */
    @Deprecated
    Page<GanttChartVO> listByProjectIdAndSearch(Map<Long, ProjectVO> projectMap,
                                                SearchVO searchVO,
                                                PageRequest pageRequest,
                                                Long organizationId,
                                                boolean orderByRank);

    Page<GanttChartVO> listByProjectIdAndSearchV2(Map<Long, ProjectVO> projectMap,
                                                  SearchParamVO searchParamVO,
                                                  PageRequest pageRequest,
                                                  Long organizationId,
                                                  boolean orderByRank);
    @Deprecated
    String getFilterSql(SearchVO searchVO);

    List<GanttChartVO> buildGanttList(Map<Long, ProjectVO> projectMap,
                                      List<Long> issueIds,
                                      List<IssueDTO> issueList,
                                      Map<Long, Long> issueEpicMap,
                                      Map<Long, IssueDTO> issueFeatureMap,
                                      List<ObjectSchemeFieldVO> displayFields,
                                      Long organizationId,
                                      GanttParentInfoVO ganttParentInfoVO);

    void saveSort(Long projectId, List<IssuePersonalSortVO> issuePersonalSorts);

    List<IssuePersonalSortVO> listLatestSort(Long projectId);

    List<IssueDTO> querySubByIssueIds(Set<Long> projectIds,
                                      List<Long> issueIds,
                                      Map<String, Object> sortMap,
                                      boolean ganttDefaultOrder,
                                      String dimension);
}
