package io.choerodon.agile.app.service;

import io.choerodon.agile.api.vo.IssueTypeVO;
import io.choerodon.agile.api.vo.business.SystemFieldOverrideConfigVO;
import io.choerodon.agile.api.vo.event.ProjectEvent;

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
}
