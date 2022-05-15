package io.choerodon.agile.api.controller.v1;

import io.choerodon.agile.api.vo.StatusNoticeSettingVO;
import io.choerodon.agile.app.service.StatusNoticeSettingService;
import io.swagger.annotations.ApiParam;
import org.hzero.core.util.Results;
import org.hzero.core.base.BaseController;
import org.hzero.starter.keyencrypt.core.Encrypt;
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
@RestController("statusNoticeSettingController.v1")
@RequestMapping("/v1/projects/{project_id}/status_notice_settings")
public class StatusNoticeSettingController extends BaseController {

    @Autowired
    private StatusNoticeSettingService statusNoticeSettingService;

    @ApiOperation(value = "消息通知明细")
    @Permission(level = ResourceLevel.ORGANIZATION)
    @GetMapping("/issue_type/{issue_type_id}/status/{status_id}")
    public ResponseEntity<StatusNoticeSettingVO> detail(@ApiParam(value = "项目id", required = true)
                                                        @PathVariable("project_id") Long projectId,
                                                        @ApiParam(value = "问题类型id", required = true)
                                                        @PathVariable("issue_type_id") @Encrypt Long issueTypeId,
                                                        @ApiParam(value = "状态id", required = true)
                                                        @PathVariable("status_id") @Encrypt Long statusId,
                                                        @ApiParam(value = "方案编码", required = true)
                                                        @RequestParam(required = false) String schemeCode) {
        return Results.success(statusNoticeSettingService.detail(projectId, issueTypeId, statusId, schemeCode));
    }

    @ApiOperation(value = "保存消息通知")
    @Permission(level = ResourceLevel.ORGANIZATION)
    @PostMapping
    public ResponseEntity<Void> save(@ApiParam(value = "项目id", required = true)
                                     @PathVariable("project_id") Long projectId,
                                     @ApiParam(value = "状态通知配置", required = true)
                                     @RequestBody StatusNoticeSettingVO statusNoticeSettingVO,
                                     @ApiParam(value = "方案编码", required = true)
                                     @RequestParam String applyType) {
        statusNoticeSettingService.save(projectId, statusNoticeSettingVO, applyType);
        return Results.success();
    }

}
