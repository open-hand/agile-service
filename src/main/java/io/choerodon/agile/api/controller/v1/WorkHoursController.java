package io.choerodon.agile.api.controller.v1;

import io.choerodon.agile.api.vo.WorkHoursCalendarVO;
import io.choerodon.agile.api.vo.WorkHoursLogVO;
import io.choerodon.agile.api.vo.WorkHoursSearchVO;
import io.choerodon.agile.api.vo.business.IssueVO;
import io.choerodon.agile.app.service.WorkHoursService;
import io.choerodon.core.domain.Page;
import io.choerodon.core.exception.CommonException;
import io.choerodon.core.iam.ResourceLevel;
import io.choerodon.mybatis.pagehelper.annotation.SortDefault;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;
import io.choerodon.mybatis.pagehelper.domain.Sort;
import io.choerodon.swagger.annotation.Permission;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import org.hzero.starter.keyencrypt.core.Encrypt;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.math.BigDecimal;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Optional;

/**
 * @author zhaotianxin
 * @date 2021-10-15 14:15
 */
@RestController
@RequestMapping(value = "/v1/projects/{project_id}/work_hours")
public class WorkHoursController {

    @Autowired
    private WorkHoursService workHoursService;

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("查询工时日志")
    @PostMapping(value = "/work_hours_log")
    public ResponseEntity<Page<WorkHoursLogVO>> pageWorkHoursLogByProjectIds(@ApiParam(value = "项目id", required = true)
                                                                             @PathVariable(name = "project_id") Long projectId,
                                                                             @RequestParam Long organizationId,
                                                                             @SortDefault(value = "creationDate", direction = Sort.Direction.DESC)
                                                                             PageRequest pageRequest,
                                                                             @RequestBody WorkHoursSearchVO workHoursSearchVO) {
        return Optional.ofNullable(workHoursService.pageWorkHoursLogByProjectIds(organizationId, Arrays.asList(projectId), pageRequest, workHoursSearchVO))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.work.hours.log.get"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("查询工时日历")
    @PostMapping(value = "/work_hours_calendar")
    public ResponseEntity<Page<WorkHoursCalendarVO>> workHoursCalendarByProjectIds(@ApiParam(value = "项目id", required = true)
                                                                                   @PathVariable(name = "project_id") Long projectId,
                                                                                   @RequestParam Long organizationId,
                                                                                   PageRequest pageRequest,
                                                                                   @RequestBody WorkHoursSearchVO workHoursSearchVO) {
        return Optional.ofNullable(workHoursService.workHoursCalendar(organizationId, Arrays.asList(projectId),pageRequest, workHoursSearchVO, false))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.work.hours.calendar.get"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("工时日历查用户的登记详情")
    @PostMapping(value = "/work_hours_calendar_info")
    public ResponseEntity<Map<String, List<WorkHoursLogVO>>> workHoursCalendarInfoByUserId(@ApiParam(value = "项目id", required = true)
                                                                                           @PathVariable(name = "project_id") Long projectId,
                                                                                           @RequestParam Long organizationId,
                                                                                           @RequestParam @Encrypt Long userId,
                                                                                           @RequestBody WorkHoursSearchVO workHoursSearchVO) {
        return Optional.ofNullable(workHoursService.workHoursCalendarInfoByUserId(organizationId, Arrays.asList(projectId), userId, workHoursSearchVO))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.work.hours.calendar.info.get"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("工时日历登记工时查询项目下的issue")
    @GetMapping(value = "/query_issue")
    public ResponseEntity<Page<IssueVO>> queryIssue(@ApiParam(value = "项目id", required = true)
                                                                                           @PathVariable(name = "project_id") Long projectId,
                                                    PageRequest pageRequest,
                                                    @RequestParam(required = false) String params) {
        return Optional.ofNullable(workHoursService.queryIssue(projectId, pageRequest, params))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.work.hours.calendar.issue.query"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("统计每天的工时总数")
    @PostMapping(value = "/count_work_hours")
    public ResponseEntity<Map<String, BigDecimal>> countWorkHours(@ApiParam(value = "项目id", required = true)
                                                    @PathVariable(name = "project_id") Long projectId,
                                                    @RequestParam(name = "organizationId") Long organizationId,
                                                    @RequestBody WorkHoursSearchVO workHoursSearchVO) {
        return Optional.ofNullable(workHoursService.countWorkHours(organizationId, Arrays.asList(projectId), workHoursSearchVO))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.count.work.hours"));
    }
}
