package io.choerodon.agile.app.service;

import java.util.List;

import org.springframework.web.context.request.RequestAttributes;

import io.choerodon.agile.api.vo.CopyConditionVO;
import io.choerodon.agile.infra.dto.business.IssueDTO;

/**
 * @author zhaotianxin
 * @date 2020-11-27 17:05
 */
public interface IssueOperateService {
    void batchDeleteIssue(Long projectId, List<Long> issueIds);

    void updateIssueStatusLinkage(Long projectId, Long issueId, IssueDTO issueDTO, String applyType, String encryptType, RequestAttributes requestAttributes);

    void asyncCloneIssueByIssueId(Long projectId,
                                  Long issueId,
                                  CopyConditionVO copyConditionVO,
                                  Long organizationId,
                                  String applyType,
                                  String asyncTraceId);
}
