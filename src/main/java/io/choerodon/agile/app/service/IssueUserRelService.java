package io.choerodon.agile.app.service;

import java.util.List;

/**
 * @author huaxin.deng@hand-china.com
 * @since 2022/4/12
 */
public interface IssueUserRelService {

    void createUserRel(Long projectId, Long issueId, List<Long> userIds, String userType);

    void updateUserRel(Long projectId, Long issueId, List<Long> userIds, String userType);
}
