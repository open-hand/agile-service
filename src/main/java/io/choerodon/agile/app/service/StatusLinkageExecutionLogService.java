package io.choerodon.agile.app.service;

import io.choerodon.agile.api.vo.ExecutionLogQueryVO;
import io.choerodon.agile.api.vo.StatusLinkageExecutionLogVO;
import io.choerodon.agile.infra.dto.StatusLinkageExecutionLogDTO;
import io.choerodon.core.domain.Page;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;

/**
 * @author zhaotianxin
 * @date 2021-08-03 9:39
 */
public interface StatusLinkageExecutionLogService {

    StatusLinkageExecutionLogDTO create(Long projectId, Long organizationId, StatusLinkageExecutionLogDTO statusLinkageExecutionLogDTO);

    void deleteByIssueId(Long projectId, Long organizationId, Long issueId);

    Page<StatusLinkageExecutionLogVO>  pageExecutionLogS(Long projectId, PageRequest pageRequest, ExecutionLogQueryVO executionLogQueryVO);
}
