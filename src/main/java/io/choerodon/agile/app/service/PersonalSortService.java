package io.choerodon.agile.app.service;

import java.util.List;

import io.choerodon.agile.api.vo.IssuePersonalSortVO;

/**
 * 用户个性化排序规则 Service
 * @author gaokuo.dai@zknow.com 2023-02-02
 */
public interface PersonalSortService {

    /**
     * 保存用户个性化排序规则
     * @param organizationId        组织ID
     * @param projectId             项目ID, 不传默认为0
     * @param businessType          业务类型
     * @param issuePersonalSorts    待保存的数据
     */
    void saveSort(Long organizationId, Long projectId, String businessType, List<IssuePersonalSortVO> issuePersonalSorts);
}
