package io.choerodon.agile.api.controller.v1;

import java.util.List;

import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.app.service.ProjectOverviewService;
import io.choerodon.agile.app.service.ReportService;
import io.choerodon.core.iam.ResourceLevel;
import io.choerodon.swagger.annotation.Permission;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import org.hzero.core.util.Results;
import org.hzero.starter.keyencrypt.core.Encrypt;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

/**
 * @author jiaxu.cui@hand-china.com 2020/6/29 下午3:21
 */
@RestController
@RequestMapping(value = "/v1/projects/{project_id}/project_overview")
public class ProjectOverviewController {

    @Autowired
    private ProjectOverviewService projectOverviewService;
    @Autowired
    private ReportService reportService;

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("查询冲刺未完成情况")
    @GetMapping("/{sprintId}/uncompleted")
    public ResponseEntity<UncompletedCountVO> selectUncompletedBySprint(@ApiParam(value = "项目id", required = true)
                                                           @PathVariable(name = "project_id") Long projectId,
                                                           @ApiParam(value = "冲刺Id", required = true)
                                                           @PathVariable @Encrypt Long sprintId) {
        return Results.success(projectOverviewService.selectUncompletedBySprint(projectId, sprintId));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("查看缺陷提出与解决情况")
    @GetMapping("/{sprintId}/issue")
    public ResponseEntity<List<IssueCompletedStatusVO>> selectIssueCountBysprint(@ApiParam(value = "项目id", required = true)
                                                                 @PathVariable(name = "project_id") Long projectId,
                                                                 @ApiParam(value = "冲刺Id", required = true)
                                                                 @PathVariable @Encrypt Long sprintId) {
        return Results.success(projectOverviewService.selectIssueCountBysprint(projectId, sprintId));
    }


    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("查看迭代统计")
    @GetMapping("/{sprintId}/sprint_statistics")
    public ResponseEntity<SprintStatisticsVO> selectSprintStatistics(@ApiParam(value = "项目id", required = true)
                                                                @PathVariable(name = "project_id") Long projectId,
                                                                     @ApiParam(value = "冲刺Id", required = true)
                                                                @PathVariable @Encrypt Long sprintId) {
        return Results.success(projectOverviewService.selectSprintStatistics(projectId, sprintId));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("查看缺陷累计趋势")
    @GetMapping("/{sprintId}/issue_count")
    public ResponseEntity<IssueCountVO> selectBugBysprint(@ApiParam(value = "项目id", required = true)
                                                           @PathVariable(name = "project_id") Long projectId,
                                                           @ApiParam(value = "冲刺Id", required = true)
                                                           @PathVariable @Encrypt Long sprintId) {
        return Results.success(reportService.selectBugBysprint(projectId, sprintId));
    }


    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("查看每人每日工作量")
    @GetMapping("/{sprintId}/one_jobs")
    public ResponseEntity<List<OneJobVO>> selectOneJobsBysprint(@ApiParam(value = "项目id", required = true)
                                                               @PathVariable(name = "project_id") Long projectId,
                                                                @ApiParam(value = "冲刺Id", required = true)
                                                               @PathVariable @Encrypt Long sprintId) {
        return Results.success(projectOverviewService.selectOneJobsBySprint(projectId, sprintId));
    }
}
