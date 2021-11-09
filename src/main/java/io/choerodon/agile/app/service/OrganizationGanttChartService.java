package io.choerodon.agile.app.service;

import io.choerodon.agile.api.vo.*;
import io.choerodon.core.domain.Page;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;

import java.util.List;

/**
 * @author superlee
 * @since 2021-10-18
 */
public interface OrganizationGanttChartService {

    Page<GanttChartVO> pagedQuery(Long organizationId, SearchVO searchVO, PageRequest pageRequest);

    List<ProjectVO> listAgileProjects(Long organizationId, String name, String code, String param);

    List<AgileIssueHeadVO> getIssueHeaderFields(Long organizationId, String schemeCode);

    List<ObjectSchemeFieldDetailVO> listCustomFields(Long organizationId);

    List<EstimatedTimeConflictVO> queryEstimatedTimeConflict(Long organizationId, SearchVO searchVO);

    Page<GanttChartVO> queryEstimatedTimeConflictDetails(Long organizationId,
                                                         SearchVO searchVO,
                                                         Long assigneeId,
                                                         PageRequest pageRequest);

    Boolean isEstimatedTimeConflicted(Long organizationId, SearchVO searchVO);
}
