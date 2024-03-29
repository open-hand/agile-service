package io.choerodon.agile.api.controller.v1;

import com.alibaba.fastjson.JSONObject;
import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.api.vo.business.IssueListVO;
import io.choerodon.agile.api.vo.report.CustomChartSearchVO;
import io.choerodon.agile.api.vo.report.CustomChartDataVO;
import io.choerodon.agile.app.service.ReportService;

import io.choerodon.agile.infra.dto.GroupDataChartDTO;
import io.choerodon.agile.infra.dto.business.GroupDataChartListDTO;
import io.choerodon.agile.infra.utils.EncryptionUtils;
import io.choerodon.core.domain.Page;
import io.choerodon.core.iam.ResourceLevel;
import io.choerodon.mybatis.pagehelper.domain.Sort;
import io.choerodon.swagger.annotation.Permission;
import io.choerodon.core.exception.CommonException;
import io.choerodon.mybatis.pagehelper.annotation.SortDefault;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;
import io.choerodon.swagger.annotation.CustomPageRequest;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import org.hzero.starter.keyencrypt.core.Encrypt;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;
import springfox.documentation.annotations.ApiIgnore;

import java.util.*;

/**
 * @author dinghuang123@gmail.com
 * @since 2018/6/19
 */
@RestController
@RequestMapping(value = "/v1/projects/{project_id}/reports")
public class ReportController {

    @Autowired
    private ReportService reportService;

    private static final String QUERY_ISSUE_ERROR = "error.issue.query";
    private static final String VERSION_LINE_CHART_ERROR = "error.version.lineChart";

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("查询冲刺对应的燃尽图报告信息")
    @PostMapping(value = "/{sprintId}/burn_down_report")
    public ResponseEntity<List<ReportIssueVO>> queryBurnDownReport(@ApiParam(value = "项目id", required = true)
                                                                   @PathVariable(name = "project_id") Long projectId,
                                                                   @ApiParam(value = "sprintId", required = true)
                                                                   @PathVariable @Encrypt Long sprintId,
                                                                   @RequestBody @Validated BurnDownSearchVO burnDownSearchVO) {
        SearchVO currentSearchVO = burnDownSearchVO.getCurrentSearchVO();
        if (Objects.nonNull(currentSearchVO)){
            EncryptionUtils.decryptSearchVO(currentSearchVO);
            burnDownSearchVO.setCurrentSearchVO(currentSearchVO);
        }
        return Optional.ofNullable(reportService.queryBurnDownReport(projectId, sprintId, burnDownSearchVO))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.report.queryBurnDownReport"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("查询燃尽图坐标信息")
    @PostMapping(value = "/{sprintId}/burn_down_report/coordinate")
    public ResponseEntity<JSONObject> queryBurnDownCoordinate(@ApiParam(value = "项目id", required = true)
                                                              @PathVariable(name = "project_id") Long projectId,
                                                              @ApiParam(value = "sprintId", required = true)
                                                              @PathVariable @Encrypt Long sprintId,
                                                              @RequestBody @Validated BurnDownSearchVO burnDownSearchVO) {
        SearchVO currentSearchVO = burnDownSearchVO.getCurrentSearchVO();
        if (Objects.nonNull(currentSearchVO)){
            EncryptionUtils.decryptSearchVO(currentSearchVO);
            burnDownSearchVO.setCurrentSearchVO(currentSearchVO);
        }
        return Optional.ofNullable(reportService.queryBurnDownCoordinate(projectId, sprintId, burnDownSearchVO))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.report.queryBurnDownCoordinate"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("查看项目累积流量图")
    @PostMapping(value = "/cumulative_flow_diagram")
    public ResponseEntity<List<CumulativeFlowDiagramVO>> queryCumulativeFlowDiagram(@ApiParam(value = "项目id", required = true)
                                                                                     @PathVariable(name = "project_id") Long projectId,
                                                                                    @ApiParam(value = "过滤条件", required = true)
                                                                                     @RequestBody CumulativeFlowFilterVO cumulativeFlowFilterVO) {
        return Optional.ofNullable(reportService.queryCumulativeFlowDiagram(projectId, cumulativeFlowFilterVO))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.report.queryCumulativeFlowDiagram"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @CustomPageRequest
    @ApiOperation(value = "根据状态查版本下issue列表")
    @GetMapping(value = "/{versionId}/issues")
    public ResponseEntity<Page<IssueListVO>> queryIssueByOptions(@ApiParam(value = "项目id", required = true)
                                                                  @PathVariable(name = "project_id") Long projectId,
                                                                 @ApiParam(value = "版本id", required = true)
                                                                  @PathVariable @Encrypt Long versionId,
                                                                 @ApiParam(value = "状态", required = true)
                                                                  @RequestParam String status,
                                                                 @ApiParam(value = "组织id", required = true)
                                                                  @RequestParam Long organizationId,
                                                                 @ApiParam(value = "类型", required = true)
                                                                  @RequestParam String type,
                                                                 @ApiParam(value = "分页信息", required = true)
                                                                  @SortDefault(value = "issueNum", direction = Sort.Direction.DESC)
                                                                  @ApiIgnore PageRequest pageRequest) {
        return Optional.ofNullable(reportService.queryIssueByOptions(projectId, versionId, status, type, pageRequest, organizationId))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException(QUERY_ISSUE_ERROR));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "版本报告图信息")
    @GetMapping(value = "/{versionId}")
    public ResponseEntity<Map<String, Object>> queryVersionLineChart(@ApiParam(value = "项目id", required = true)
                                                                     @PathVariable(name = "project_id") Long projectId,
                                                                     @ApiParam(value = "版本id", required = true)
                                                                     @PathVariable @Encrypt Long versionId,
                                                                     @ApiParam(value = "统计类型", required = true)
                                                                     @RequestParam String type) {
        return Optional.ofNullable(reportService.queryVersionLineChart(projectId, versionId, type))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException(VERSION_LINE_CHART_ERROR));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "速度图")
    @GetMapping(value = "/velocity_chart")
    public ResponseEntity<List<VelocitySprintVO>> queryVelocityChart(@ApiParam(value = "项目id", required = true)
                                                                      @PathVariable(name = "project_id") Long projectId,
                                                                     @ApiParam(value = "统计类型", required = true)
                                                                      @RequestParam String type) {
        return Optional.ofNullable(reportService.queryVelocityChart(projectId, type))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.velocityChart.get"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("查询饼图")
    @GetMapping(value = "/pie_chart")
    public ResponseEntity<List<PieChartVO>> queryPieChart(@ApiParam(value = "项目id", required = true)
                                                           @PathVariable(name = "project_id") Long projectId,
                                                          @ApiParam(value = "字段名称:assignee、component、typeCode、" +
                                                                   "version、priorityCode、statusCode、sprint、epic、resolution、label", required = true)
                                                           @RequestParam String fieldName,
                                                          @ApiParam(value = "组织id", required = true)
                                                           @RequestParam Long organizationId,
                                                          @ApiParam(value = "开始时间 yyyy-MM-dd HH:mm:ss")
                                                           @RequestParam(required = false) @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss") Date startDate,
                                                          @ApiParam(value = "结束时间 yyyy-MM-dd HH:mm:ss")
                                                           @RequestParam(required = false) @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss") Date endDate,
                                                          @ApiParam(value = "冲刺id")
                                                           @RequestParam(required = false) @Encrypt Long sprintId,
                                                           @ApiParam(value = "版本id")
                                                           @RequestParam(required = false) @Encrypt Long versionId,
                                                           @RequestParam(required = false) @Encrypt Long customFieldId,
                                                           @ApiParam(value = "状态id") @RequestParam(required = false) @Encrypt Long statusId) {
        return Optional.ofNullable(reportService.queryPieChart(projectId, fieldName, organizationId, startDate, endDate, sprintId, versionId, statusId, customFieldId))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.report.queryPieChart"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "史诗图")
    @GetMapping(value = "/epic_chart")
    public ResponseEntity<List<GroupDataChartDTO>> queryEpicChart(@ApiParam(value = "项目id", required = true)
                                                                 @PathVariable(name = "project_id") Long projectId,
                                                                  @ApiParam(value = "epic id", required = true)
                                                                 @RequestParam @Encrypt Long epicId,
                                                                  @ApiParam(value = "统计类型", required = true)
                                                                 @RequestParam String type) {
        return Optional.ofNullable(reportService.queryEpicChart(projectId, epicId, type))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.epicChart.get"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "史诗图问题列表")
    @GetMapping(value = "/epic_issue_list")
    public ResponseEntity<List<GroupDataChartListDTO>> queryEpicChartList(@ApiParam(value = "项目id", required = true)
                                                                         @PathVariable(name = "project_id") Long projectId,
                                                                          @ApiParam(value = "epic id", required = true)
                                                                         @RequestParam @Encrypt Long epicId,
                                                                          @ApiParam(value = "组织id", required = true)
                                                                         @RequestParam Long organizationId) {
        return Optional.ofNullable(reportService.queryEpicChartList(projectId, epicId, organizationId))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.epicChartList.get"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "版本图重构api")
    @GetMapping(value = "/version_chart")
    public ResponseEntity<List<GroupDataChartDTO>> queryVersionChart(@ApiParam(value = "项目id", required = true)
                                                                    @PathVariable(name = "project_id") Long projectId,
                                                                     @ApiParam(value = "version id", required = true)
                                                                    @RequestParam @Encrypt Long versionId,
                                                                     @ApiParam(value = "统计类型", required = true)
                                                                    @RequestParam String type) {
        return Optional.ofNullable(reportService.queryVersionChart(projectId, versionId, type))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.versionChart.get"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "版本图问题列表重构api")
    @GetMapping(value = "/version_issue_list")
    public ResponseEntity<List<GroupDataChartListDTO>> queryVersionChartList(@ApiParam(value = "项目id", required = true)
                                                                            @PathVariable(name = "project_id") Long projectId,
                                                                             @ApiParam(value = "version id", required = true)
                                                                            @RequestParam @Encrypt Long versionId,
                                                                             @ApiParam(value = "组织id", required = true)
                                                                            @RequestParam Long organizationId) {
        return Optional.ofNullable(reportService.queryVersionChartList(projectId, versionId, organizationId))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.versionChartList.get"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("Epic和版本燃耗图坐标信息")
    @GetMapping(value = "/burn_down_coordinate_type/{id}")
    public ResponseEntity<List<BurnDownReportCoordinateVO>> queryBurnDownCoordinateByType(@ApiParam(value = "项目id", required = true)
                                                                                           @PathVariable(name = "project_id") Long projectId,
                                                                                          @ApiParam(value = "id", required = true)
                                                                                           @PathVariable @Encrypt Long id,
                                                                                          @ApiParam(value = "类型:Epic/Version", required = true)
                                                                                           @RequestParam String type) {
        return Optional.ofNullable(reportService.queryBurnDownCoordinateByType(projectId, id, type))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.report.queryBurnDownCoordinateByType"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("Epic和版本燃耗图报告信息")
    @GetMapping(value = "/burn_down_report_type/{id}")
    public ResponseEntity<BurnDownReportVO> queryBurnDownReportByType(@ApiParam(value = "项目id", required = true)
                                                                       @PathVariable(name = "project_id") Long projectId,
                                                                      @ApiParam(value = "id", required = true)
                                                                       @PathVariable @Encrypt Long id,
                                                                      @ApiParam(value = "类型:Epic/Version", required = true)
                                                                       @RequestParam String type,
                                                                      @ApiParam(value = "组织id", required = true)
                                                                       @RequestParam Long organizationId) {
        return Optional.ofNullable(reportService.queryBurnDownReportByType(projectId, id, type, organizationId))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.report.queryBurnDownReportByType"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("问题类型分布图")
    @GetMapping(value = "/issue_type_distribution_chart")
    public ResponseEntity<List<IssueTypeDistributionChartVO>> queryIssueTypeDistributionChart(@ApiParam(value = "项目id", required = true)
                                                                                               @PathVariable(name = "project_id") Long projectId) {
        return Optional.ofNullable(reportService.queryIssueTypeDistributionChart(projectId))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.report.queryIssueTypeDistributionChart"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("版本进度图，排序前5个版本")
    @GetMapping(value = "/version_progress_chart")
    public ResponseEntity<List<IssueTypeDistributionChartVO>> queryVersionProgressChart(@ApiParam(value = "项目id", required = true)
                                                                                         @PathVariable(name = "project_id") Long projectId) {
        return Optional.ofNullable(reportService.queryVersionProgressChart(projectId))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.report.queryVersionProgressChart"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("问题优先级分布图")
    @GetMapping(value = "/issue_priority_distribution_chart")
    public ResponseEntity<List<IssuePriorityDistributionChartVO>> queryIssuePriorityDistributionChart(@ApiParam(value = "项目id", required = true)
                                                                                                       @PathVariable(name = "project_id") Long projectId,
                                                                                                      @ApiParam(value = "组织id", required = true)
                                                                                                       @RequestParam Long organizationId) {
        return Optional.ofNullable(reportService.queryIssuePriorityDistributionChart(projectId, organizationId))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.report.queryIssuePriorityDistributionChart"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("查询自定义报表数据")
    @PostMapping(value = "/custom_chart")
    public ResponseEntity<CustomChartDataVO> queryCustomChartData(
            @ApiParam(value = "项目id", required = true)
            @PathVariable(name = "project_id") Long projectId,
            @ApiParam(value = "组织id", required = true)
            @RequestParam Long organizationId,
            @Validated @RequestBody CustomChartSearchVO customChartSearchVO) {
        EncryptionUtils.decryptSearchVO(customChartSearchVO.getExtendSearchVO());
        EncryptionUtils.decryptSearchVO(customChartSearchVO.getSearchVO());
        return Optional.ofNullable(reportService.queryCustomChartData(customChartSearchVO, projectId, organizationId))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.report.queryCustomChart"));
    }
}
