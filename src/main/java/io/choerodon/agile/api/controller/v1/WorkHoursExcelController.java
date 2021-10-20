package io.choerodon.agile.api.controller.v1;

import io.choerodon.agile.api.vo.WorkHoursSearchVO;
import io.choerodon.agile.app.service.WorkHoursExcelService;
import io.choerodon.core.iam.ResourceLevel;
import io.choerodon.swagger.annotation.Permission;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

import java.util.Arrays;

/**
 * @author zhaotianxin
 * @date 2021-10-19 14:09
 */
@RestController
@RequestMapping(value = "/v1/projects/{project_id}/work_hours")
public class WorkHoursExcelController {
    @Autowired
    private WorkHoursExcelService workHoursExcelService;

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("导出工时日历")
    @PostMapping(value = "/export_work_hours_log")
    public void download(@ApiParam(value = "项目id", required = true)
                         @PathVariable(name = "project_id") Long projectId,
                         @ApiParam(value = "组织id", required = true)
                         @RequestParam Long organizationId,
                         @RequestBody WorkHoursSearchVO workHoursSearchVO) {
        workHoursExcelService.exportWorkHoursLogOnProjectLevel(organizationId, Arrays.asList(projectId), workHoursSearchVO, (ServletRequestAttributes) RequestContextHolder.currentRequestAttributes());
    }
}
