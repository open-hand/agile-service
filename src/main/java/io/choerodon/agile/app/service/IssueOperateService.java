package io.choerodon.agile.app.service;

import java.util.List;

/**
 * @author zhaotianxin
 * @date 2020-11-27 17:05
 */
public interface IssueOperateService {
    void batchDeleteIssue(Long projectId, List<Long> issueIds);
}
