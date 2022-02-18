package io.choerodon.agile.app.service;

import io.choerodon.agile.api.vo.GanttChartVO;
import io.choerodon.agile.api.vo.IssueTypeVO;
import io.choerodon.agile.api.vo.ProjectVO;
import io.choerodon.agile.api.vo.SearchVO;
import io.choerodon.agile.api.vo.business.IssueCreateVO;
import io.choerodon.agile.api.vo.business.IssueVO;
import io.choerodon.agile.api.vo.business.SystemFieldOverrideConfigVO;
import io.choerodon.agile.api.vo.event.ProjectEvent;
import io.choerodon.core.domain.Page;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;

import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * @author superlee
 * @since 2022-02-15
 */
public interface AgileWaterfallService {

    List<String> getSystemFieldContext(String code);

    Map<String, List<SystemFieldOverrideConfigVO>> querySystemFieldOverrideConfig();

    void initProject(ProjectEvent projectEvent, Set<String> codes);

    List<IssueTypeVO> filterWaterfallIssueTypes(List<IssueTypeVO> issueTypes, List<Long> issueTypeIds);

    Long initWaterfallStateMachine(Long organizationId, ProjectEvent projectEvent);

    void checkBeforeCreateIssue(IssueCreateVO issueCreateVO, String applyType);

    void handlerWaterfallAfterCreateIssue(Long projectId, Long issueId, IssueCreateVO issueCreateVO);

    void waterfallIssueDetailDTOToVO(IssueVO issueVO);

    Page<GanttChartVO> pagedQuery(ProjectVO project,
                                  SearchVO searchVO,
                                  PageRequest pageRequest);
}
