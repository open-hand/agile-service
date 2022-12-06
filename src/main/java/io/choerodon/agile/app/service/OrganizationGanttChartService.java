package io.choerodon.agile.app.service;

import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.api.vo.search.SearchParamVO;
import io.choerodon.core.domain.Page;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;

import java.util.List;

/**
 * @author superlee
 * @since 2021-10-18
 */
public interface OrganizationGanttChartService {

    /**
     * @param organizationId
     * @param searchVO
     * @param pageRequest
     * @return
     * @see OrganizationGanttChartService#pagedQueryV2(Long, SearchParamVO, PageRequest)
     * @see
     */
    @Deprecated
    Page<GanttChartVO> pagedQuery(Long organizationId, SearchVO searchVO, PageRequest pageRequest);

    Page<GanttChartVO> pagedQueryV2(Long organizationId, SearchParamVO searchParamVO, PageRequest pageRequest);

    List<ProjectVO> listAgileProjects(Long organizationId, String name, String code, String param);

    List<AgileIssueHeadVO> getIssueHeaderFields(Long organizationId, String schemeCode);

    List<ObjectSchemeFieldDetailVO> listCustomFields(Long organizationId);

    /**
     * @param organizationId
     * @param searchVO
     * @return
     * @see OrganizationGanttChartService#queryEstimatedTimeConflictV2(Long, SearchParamVO)
     */
    @Deprecated
    List<EstimatedTimeConflictVO> queryEstimatedTimeConflict(Long organizationId, SearchVO searchVO);

    List<EstimatedTimeConflictVO> queryEstimatedTimeConflictV2(Long organizationId, SearchParamVO searchParamVO);

    /**
     * @param organizationId
     * @param searchVO
     * @param assigneeId
     * @param pageRequest
     * @return
     * @see OrganizationGanttChartService#queryEstimatedTimeConflictDetailsV2(Long, SearchParamVO, Long, PageRequest)
     */
    @Deprecated
    Page<GanttChartVO> queryEstimatedTimeConflictDetails(Long organizationId,
                                                         SearchVO searchVO,
                                                         Long assigneeId,
                                                         PageRequest pageRequest);

    Page<GanttChartVO> queryEstimatedTimeConflictDetailsV2(Long organizationId,
                                                           SearchParamVO searchParamVO,
                                                           Long assigneeId,
                                                           PageRequest pageRequest);

    /**
     * @param organizationId
     * @param searchVO
     * @return
     * @see OrganizationGanttChartService#isEstimatedTimeConflictedV2(Long, SearchParamVO)
     */
    @Deprecated
    Boolean isEstimatedTimeConflicted(Long organizationId, SearchVO searchVO);

    Boolean isEstimatedTimeConflictedV2(Long organizationId, SearchParamVO searchParamVO);
}
