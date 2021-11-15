package io.choerodon.agile.app.service;

import io.choerodon.agile.api.vo.IssuePredecessorVO;
import io.choerodon.agile.api.vo.SearchVO;
import io.choerodon.agile.api.vo.business.IssueListFieldKVVO;
import io.choerodon.agile.infra.dto.LookupValueDTO;
import io.choerodon.core.domain.Page;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;

import java.util.List;

/**
 * @author superlee
 * @since 2021-11-10
 */
public interface IssuePredecessorService {

    List<LookupValueDTO> queryPredecessorTypes(Long projectId);

    void updatePredecessors(Long projectId,
                            List<IssuePredecessorVO> issuePredecessors,
                            Long currentIssueId);

    void addSelfNode(Long projectId, Long issueId);

    void deleteNode(Long projectId, Long issueId);

    Page<IssueListFieldKVVO> pagedQueryEnabledIssues(Long organizationId,
                                                     Long projectId,
                                                     SearchVO searchVO,
                                                     PageRequest pageRequest,
                                                     Long currentIssueId);
}
