package io.choerodon.agile.api.controller.v1;

import io.choerodon.agile.api.vo.StatusNoticeSettingVO;
import io.choerodon.agile.app.service.StatusNoticeSettingService;
import org.hzero.core.util.Results;
import org.hzero.core.base.BaseController;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import io.choerodon.core.iam.ResourceLevel;
import io.choerodon.swagger.annotation.Permission;
import io.swagger.annotations.ApiOperation;

/**
 * 消息通知 管理 API
 *
 * @author choerodon@choerodon.cn 2020-08-12 11:41:01
 */
@RestController(" statusNoticeSettingController.v1")
@RequestMapping("/v1/projects/{project_id}/status_notice_settings")
public class StatusNoticeSettingController extends BaseController {

    @Autowired
    private StatusNoticeSettingService statusNoticeSettingService;

    @ApiOperation(value = "消息通知明细")
    @Permission(level = ResourceLevel.ORGANIZATION)
    @GetMapping("/issue_type/{issue_type_id}/status/{status_id}")
    public ResponseEntity<StatusNoticeSettingVO> detail(@PathVariable("project_id") Long projectId,
                                                        @PathVariable("issue_type_id") Long issueTypeId,
                                                        @PathVariable("status_id") Long statusId) {
        return Results.success(statusNoticeSettingService.detail(projectId, issueTypeId, statusId));
    }

    @ApiOperation(value = "保存消息通知")
    @Permission(level = ResourceLevel.ORGANIZATION)
    @PostMapping
    public ResponseEntity<Void> save(@PathVariable("project_id") Long projectId,
                                                        @RequestBody StatusNoticeSettingVO StatusNoticeSettingVO) {
        validObject(StatusNoticeSettingVO);
        statusNoticeSettingService.save(projectId, StatusNoticeSettingVO);
        return Results.success();
    }

}
