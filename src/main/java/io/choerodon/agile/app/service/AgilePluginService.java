package io.choerodon.agile.app.service;

import io.choerodon.agile.infra.dto.IssueConvertDTO;

/**
 * @author zhaotianxin
 * @date 2020-10-12 10:35
 */
public interface AgilePluginService {
    String getSystemFieldContext(String code);

    void deleteIssueForBusiness(IssueConvertDTO issueConvertDTO);
}
