package io.choerodon.agile.infra.mapper;

import io.choerodon.agile.api.vo.ExecutionLogQueryVO;
import io.choerodon.agile.api.vo.StatusLinkageExecutionLogVO;
import io.choerodon.agile.infra.dto.StatusLinkageExecutionLogDTO;
import io.choerodon.mybatis.common.BaseMapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

/**
 * @author zhaotianxin
 * @date 2021-08-03 9:38
 */
public interface StatusLinkageExecutionLogMapper extends BaseMapper<StatusLinkageExecutionLogDTO> {

    void deleteByIssueId(@Param("projectId") Long projectId, @Param("organizationId") Long organizationId, @Param("issueId") Long issueId);

    List<StatusLinkageExecutionLogVO> listExecutionLog(@Param("projectId") Long projectId, @Param("organizationId") Long organizationId, @Param("executionLogQueryVO") ExecutionLogQueryVO executionLogQueryVO);
}
