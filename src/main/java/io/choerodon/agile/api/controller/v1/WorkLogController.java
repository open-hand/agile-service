package io.choerodon.agile.api.controller.v1;

import java.util.List;
import java.util.Optional;

import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import io.choerodon.agile.api.vo.IssueWorkTimeCountVO;
import io.choerodon.agile.api.vo.WorkLogVO;
import io.choerodon.agile.app.service.WorkLogService;
import io.choerodon.core.exception.CommonException;
import io.choerodon.core.iam.ResourceLevel;
import io.choerodon.swagger.annotation.Permission;

import org.hzero.core.util.Results;
import org.hzero.starter.keyencrypt.core.Encrypt;

/**
 * Created by HuangFuqiang@choerodon.io on 2018/5/18.
 * Email: fuqianghuang01@gmail.com
 */
@RestController
@RequestMapping(value = "/v1/projects/{project_id}/work_log")
public class WorkLogController {

    @Autowired
    private WorkLogService workLogService;

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("创建work log")
    @PostMapping
    public ResponseEntity<WorkLogVO> createWorkLog(@ApiParam(value = "项目id", required = true)
                                                    @PathVariable(name = "project_id") Long projectId,
                                                   @ApiParam(value = "work log object", required = true)
                                                    @RequestBody WorkLogVO workLogVO) {
        return Optional.ofNullable(workLogService.createWorkLog(projectId, workLogVO))
                .map(Results::created)
                .orElseThrow(() -> new CommonException("error.workLog.create"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("修改work log")
    @PatchMapping(value = "/{logId}")
    public ResponseEntity<WorkLogVO> updateWorkLog(@ApiParam(value = "项目id", required = true)
                                                   @PathVariable(name = "project_id") Long projectId,
                                                   @ApiParam(value = "log id", required = true)
                                                   @PathVariable @Encrypt Long logId,
                                                   @ApiParam(value = "work log object", required = true)
                                                   @RequestBody WorkLogVO workLogVO) {
        return Optional.ofNullable(workLogService.updateWorkLog(projectId, logId, workLogVO))
                .map(Results::created)
                .orElseThrow(() -> new CommonException("error.workLog.update"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("删除work log")
    @DeleteMapping(value = "/{logId}")
    public ResponseEntity<Void> deleteWorkLog(@ApiParam(value = "项目id", required = true)
                                        @PathVariable(name = "project_id") Long projectId,
                                        @ApiParam(value = "log id", required = true)
                                        @PathVariable @Encrypt Long logId) {
        workLogService.deleteWorkLog(projectId, logId);
        return Results.success();
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("根据logId查询work log")
    @GetMapping(value = "/{logId}")
    public ResponseEntity<WorkLogVO> queryWorkLogById(@ApiParam(value = "项目id", required = true)
                                           @PathVariable(name = "project_id") Long projectId,
                                           @ApiParam(value = "log id", required = true)
                                           @PathVariable @Encrypt Long logId) {
        return Optional.ofNullable(workLogService.queryWorkLogById(projectId, logId))
                .map(Results::success)
                .orElseThrow(() -> new CommonException("error.workLog.get"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("根据issue id查询work log列表")
    @GetMapping(value = "/issue/{issueId}")
    public ResponseEntity<List<WorkLogVO>> queryWorkLogListByIssueId(@ApiParam(value = "项目id", required = true)
                                                                      @PathVariable(name = "project_id") Long projectId,
                                                                     @ApiParam(value = "issue id", required = true)
                                                                      @PathVariable @Encrypt Long issueId) {
        return Optional.ofNullable(workLogService.queryWorkLogListByIssueId(projectId, issueId))
                .map(Results::success)
                .orElseThrow(() -> new CommonException("error.workLogList.get"));
    }


    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("统计工时日志数量")
    @GetMapping(value = "/issue/{issueId}/count_work_time")
    public ResponseEntity<IssueWorkTimeCountVO> countWorkTime(@ApiParam(value = "项目id", required = true)
                                                              @PathVariable(name = "project_id") Long projectId,
                                                              @ApiParam(value = "issueId", required = true)
                                                              @PathVariable @Encrypt Long issueId) {
        return  ResponseEntity.ok(workLogService.countWorkTime(projectId, issueId));
    }

}
