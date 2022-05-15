package io.choerodon.agile.api.controller.v1;

import io.choerodon.agile.api.vo.ExecutionLogQueryVO;
import io.choerodon.agile.api.vo.StatusLinkageExecutionLogVO;
import io.choerodon.agile.app.service.StatusLinkageExecutionLogService;
import io.choerodon.core.domain.Page;
import io.choerodon.core.exception.CommonException;
import io.choerodon.core.iam.ResourceLevel;
import io.choerodon.mybatis.pagehelper.annotation.SortDefault;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;
import io.choerodon.mybatis.pagehelper.domain.Sort;
import io.choerodon.swagger.annotation.Permission;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Optional;

/**
 * @author zhaotianxin
 * @date 2021-08-04 10:18
 */
@RestController
@RequestMapping("/v1/projects/{project_id}/status_linkage_execution_log")
public class StatusLinkageExecutionLogController {

    @Autowired
    private StatusLinkageExecutionLogService statusLinkageExecutionLogService;

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("查询项目下的状态联动执行日志")
    @PostMapping("/list")
    public ResponseEntity<Page<StatusLinkageExecutionLogVO>> pageExecutionLogS(@ApiParam(value = "项目id", required = true)
                                                                               @PathVariable(name = "project_id") Long projectId,
                                                                               @SortDefault(value = "id", direction = Sort.Direction.DESC)
                                                                                       PageRequest pageRequest,
                                                                               @ApiParam(value = "执行记录查询条件", required = true)
                                                                               @RequestBody ExecutionLogQueryVO executionLogQueryVO) {
        return Optional.ofNullable(statusLinkageExecutionLogService.pageExecutionLogS(projectId, pageRequest, executionLogQueryVO))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.status.linkage.execution.log.query"));
    }
}
