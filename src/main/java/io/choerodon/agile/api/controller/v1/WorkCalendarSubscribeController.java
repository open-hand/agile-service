package io.choerodon.agile.api.controller.v1;

import io.choerodon.agile.app.service.WorkCalendarSubscribeService;
import io.choerodon.core.iam.ResourceLevel;
import io.choerodon.swagger.annotation.Permission;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import org.hzero.core.util.Results;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletResponse;

/**
 * @author huaxin.deng@hand-china.com 2021-10-11 14:21:54
 */
@RestController
@RequestMapping(value = "/v1/organizations/{organization_id}/work_calendar_subscribe")
public class WorkCalendarSubscribeController {

    @Autowired
    private WorkCalendarSubscribeService workCalendarSubscribeService;

    @Permission(permissionLogin = true, level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "订阅工作日历")
    @GetMapping
    public ResponseEntity<String> subscribe(@ApiParam(value = "组织ID", required = true)
                                            @PathVariable("organization_id") Long organizationId) {
        return Results.success(workCalendarSubscribeService.subscribe(organizationId));
    }

    @Permission(permissionPublic = true)
    @ApiOperation(value = "获取文件")
    @GetMapping("/{uuid}")
    public ResponseEntity<byte[]> downloadFile(@ApiParam(value = "组织ID", required = true)
                                               @PathVariable("organization_id") Long organizationId,
                                               @ApiParam(value = "uuid", required = true)
                                               @PathVariable("uuid") String uuid,
                                               HttpServletResponse httpResponse) {
        return workCalendarSubscribeService.downloadFile(organizationId, uuid, httpResponse);
    }

    @Permission(permissionLogin = true, level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "订阅查询")
    @GetMapping("/query/uuid")
    public ResponseEntity<String> query(@ApiParam(value = "组织ID", required = true)
                                        @PathVariable("organization_id") Long organizationId) {
        return Results.success(workCalendarSubscribeService.query(organizationId));
    }

}
