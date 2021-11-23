package io.choerodon.agile.api.controller.v1;

import io.choerodon.agile.api.vo.SearchVO;
import io.choerodon.agile.api.vo.WorkHoursSearchVO;
import io.choerodon.agile.app.service.WorkHoursExcelService;
import io.choerodon.agile.infra.utils.EncryptionUtils;
import io.choerodon.core.iam.ResourceLevel;
import io.choerodon.swagger.annotation.Permission;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

import javax.servlet.http.HttpServletResponse;
import java.util.Arrays;


/**
 * @author zhaotianxin
 * @date 2021-10-19 14:09
 */
@RestController
@RequestMapping(value = "/v1/organizations/{organization_id}/work_hours")
public class WorkHoursExcelOrgController {
    @Autowired
    private WorkHoursExcelService workHoursExcelService;

    @Permission(level = ResourceLevel.ORGANIZATION, permissionLogin = true)
    @ApiOperation("导出工时日历")
    @PostMapping(value = "/export_work_hours_log")
    public void download(@ApiParam(value = "项目id", required = true)
                         @PathVariable(name = "organization_id") Long organizationId,
                         @RequestBody WorkHoursSearchVO workHoursSearchVO) {
        workHoursExcelService.exportWorkHoursLogOnOrganizationLevel(organizationId, workHoursSearchVO, (ServletRequestAttributes) RequestContextHolder.currentRequestAttributes());
    }

    @Permission(level = ResourceLevel.ORGANIZATION, permissionLogin = true)
    @ApiOperation("导出工时日历")
    @PostMapping(value = "/export_work_hours_calendar")
    public void exportWorkHoursCalendar(@ApiParam(value = "组织id", required = true)
                                        @PathVariable(name = "organization_id") Long organizationId,
                                        @RequestBody WorkHoursSearchVO workHoursSearchVO) {
        workHoursExcelService.exportWorkHoursCalendarOnOrganizationLevel(organizationId, workHoursSearchVO, (ServletRequestAttributes) RequestContextHolder.currentRequestAttributes(), true);
    }

    @Permission(level = ResourceLevel.ORGANIZATION, permissionLogin = true)
    @ApiOperation("导出工作项工时")
    @PostMapping(value = "/export_issue_work_hours")
    public void exportIssueWorkHours(@ApiParam(value = "组织id", required = true)
                                     @PathVariable(name = "organization_id") Long organizationId,
                                     @RequestParam(required = false, defaultValue = "false") Boolean containsSubIssue,
                                     @RequestBody SearchVO searchVO) {
        EncryptionUtils.decryptSearchVO(searchVO);
        workHoursExcelService.exportIssueWorkHoursOnOrganizationLevel(organizationId,(ServletRequestAttributes) RequestContextHolder.currentRequestAttributes(), true, searchVO, containsSubIssue);
    }
}
