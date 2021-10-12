package io.choerodon.agile.api.controller.v1;

import io.choerodon.agile.api.vo.WorkCalendarRefVO;
import io.choerodon.agile.api.vo.WorkItemSearchVO;
import io.choerodon.agile.api.vo.WorkItemVO;
import io.choerodon.agile.app.service.WorkCalenderService;
import io.choerodon.core.exception.CommonException;
import io.choerodon.core.iam.ResourceLevel;
import io.choerodon.swagger.annotation.Permission;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.Optional;

/**
 * @author zhaotianxin
 * @date 2021-10-11 15:47
 */
@RestController
@RequestMapping(value = "/v1/organizations/{organization_id}/work_calender")
public class WorkCalenderController {
    @Autowired
    private WorkCalenderService workCalenderService;

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("查询项目下活跃冲刺中我经办的issue")
    @PostMapping(value = "/query_parent_issue")
    public ResponseEntity<List<WorkItemVO>> queryAssigneeParentIssueList(@ApiParam(value = "项目id", required = true)
                                                                         @PathVariable(name = "organization_id") Long organizationId,
                                                                         @RequestBody WorkItemSearchVO workItemSearchVO) {
        return Optional.ofNullable(workCalenderService.queryAssigneeParentIssueList(organizationId, workItemSearchVO))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.workCalendar.get"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("查询项目下我经办的issue")
    @PostMapping(value = "/query_assignee_issue")
    public ResponseEntity<List<WorkItemVO>> queryAssigneeIssueList(@ApiParam(value = "项目id", required = true)
                                                                         @PathVariable(name = "organization_id") Long organizationId,
                                                                         @RequestBody WorkItemSearchVO workItemSearchVO) {
        return Optional.ofNullable(workCalenderService.queryAssigneeIssueList(organizationId, workItemSearchVO))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.workCalendar.get"));
    }
}
