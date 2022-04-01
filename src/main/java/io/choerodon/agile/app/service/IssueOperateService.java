package io.choerodon.agile.app.service;

import io.choerodon.agile.infra.dto.business.IssueDTO;
import org.springframework.web.context.request.RequestAttributes;

import java.util.List;

/**
 * @author zhaotianxin
 * @date 2020-11-27 17:05
 */
public interface IssueOperateService {
    void batchDeleteIssue(Long projectId, List<Long> issueIds);

    void updateIssueStatusLinkage(Long projectId, Long issueId, IssueDTO issueDTO, String applyType, String encryptType, RequestAttributes requestAttributes);
}
