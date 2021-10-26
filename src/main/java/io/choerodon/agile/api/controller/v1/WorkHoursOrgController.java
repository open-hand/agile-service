package io.choerodon.agile.api.controller.v1;

import io.choerodon.agile.api.vo.WorkHoursCalendarVO;
import io.choerodon.agile.api.vo.WorkHoursLogVO;
import io.choerodon.agile.api.vo.WorkHoursSearchVO;
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

import java.util.List;
import java.util.Map;
import java.util.Optional;

/**
 * @author zhaotianxin
 * @date 2021-10-15 14:15
 */
@RestController
@RequestMapping(value = "/v1/organizations/{organization_id}/work_hours")
public class WorkHoursOrgController {

    @Autowired
    private WorkHoursService workHoursService;

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("查询工时日志")
    @PostMapping(value = "/work_hours_log")
    public ResponseEntity<Page<WorkHoursLogVO>> pageWorkHoursLogByOrgId(@ApiParam(value = "组织id", required = true)
                                                                        @PathVariable(name = "organization_id") Long organizationId,
                                                                        @SortDefault(value = "creationDate", direction = Sort.Direction.DESC)
                                                                                PageRequest pageRequest,
                                                                        @RequestBody WorkHoursSearchVO workHoursSearchVO) {
        return Optional.ofNullable(workHoursService.pageWorkHoursLogByOrgId(organizationId, pageRequest, workHoursSearchVO))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.workCalendar.get"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("查询工时日历")
    @PostMapping(value = "/work_hours_calendar")
    public ResponseEntity<List<WorkHoursCalendarVO>> workHoursCalendarByProjectIds(@ApiParam(value = "组织Id", required = true)
                                                                                   @PathVariable(name = "organization_id") Long organizationId,
                                                                                   @RequestBody WorkHoursSearchVO workHoursSearchVO) {
        return Optional.ofNullable(workHoursService.workHoursCalendarByOrg(organizationId, workHoursSearchVO))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.work.hours.calendar.get"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("工时日历查用户的登记详情")
    @PostMapping(value = "/work_hours_calendar_info")
    public ResponseEntity<Map<String, List<WorkHoursLogVO>>> workHoursCalendarOrgInfoByUserId(@ApiParam(value = "组织id", required = true)
                                                                                              @PathVariable(name = "organization_id") Long organizationId,
                                                                                              @RequestParam @Encrypt Long userId,
                                                                                              @RequestBody WorkHoursSearchVO workHoursSearchVO) {
        return Optional.ofNullable(workHoursService.workHoursCalendarOrgInfoByUserId(organizationId, userId, workHoursSearchVO))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.work.hours.calendar.info.get"));
    }
}
