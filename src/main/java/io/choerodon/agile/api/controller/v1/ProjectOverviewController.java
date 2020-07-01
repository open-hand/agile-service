package io.choerodon.agile.api.controller.v1;

import io.choerodon.agile.api.vo.BoardColumnVO;
import io.choerodon.agile.api.vo.IssueCountVO;
import io.choerodon.agile.api.vo.SprintStatisticsVO;
import io.choerodon.agile.api.vo.UncompletedCountVO;
import io.choerodon.agile.app.service.ProjectOverviewService;
import io.choerodon.core.iam.ResourceLevel;
import io.choerodon.swagger.annotation.Permission;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import org.hzero.core.util.Results;
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

    @Permission(level = ResourceLevel.PROJECT)
    @ApiOperation("查询冲刺未完成情况")
    @GetMapping("/{sprintId}/uncompleted")
    public ResponseEntity<UncompletedCountVO> selectUncompletedBySprint(@ApiParam(value = "项目id", required = true)
                                                           @PathVariable(name = "project_id") Long projectId,
                                                           @ApiParam(value = "冲刺Id", required = true)
                                                           @PathVariable Long sprintId) {
        return Results.success(projectOverviewService.selectUncompletedBySprint(projectId, sprintId));
    }

    // 燃尽图 https://api.choerodon.com.cn/agile/v1/projects/1528/reports/7264/burn_down_report/coordinate?type=remainingEstimatedTime

    @Permission(level = ResourceLevel.PROJECT)
    @ApiOperation("查看缺陷提出与解决情况")
    @GetMapping("/{sprintId}/issue")
    public ResponseEntity<IssueCountVO> selectIssueCountBysprint(@ApiParam(value = "项目id", required = true)
                                                                 @PathVariable(name = "project_id") Long projectId,
                                                                 @ApiParam(value = "冲刺Id", required = true)
                                                                 @PathVariable Long sprintId) {
        return Results.success(projectOverviewService.selectIssueCountBysprint(projectId, sprintId));
    }


    @Permission(level = ResourceLevel.PROJECT)
    @ApiOperation("查看迭代统计")
    @GetMapping("/{sprintId}/sprint_statistics")
    public ResponseEntity<SprintStatisticsVO> selectSprintStatistics(@ApiParam(value = "项目id", required = true)
                                                                @PathVariable(name = "project_id") Long projectId,
                                                                     @ApiParam(value = "冲刺Id", required = true)
                                                                @PathVariable Long sprintId) {
        return Results.success(projectOverviewService.selectSprintStatistics(projectId, sprintId));
    }


    @Permission(level = ResourceLevel.PROJECT)
    @ApiOperation("查看每人每日工作量")
    @GetMapping("/{sprintId}/one_jobs")
    public ResponseEntity<BoardColumnVO> selectOneJobsBysprint(@ApiParam(value = "项目id", required = true)
                                                               @PathVariable(name = "project_id") Long projectId,
                                                               @ApiParam(value = "冲刺Id", required = true)
                                                               @PathVariable Long sprintId) {
        return Results.success();
    }
}
