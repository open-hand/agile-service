package io.choerodon.agile.api.controller.v1;

import io.choerodon.agile.app.service.StatusNoticeSettingService;
import io.choerodon.agile.infra.dto.StatusNoticeSetting;
import org.hzero.core.util.Results;
import org.hzero.core.base.BaseController;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.hzero.mybatis.helper.SecurityTokenHelper;

import io.choerodon.core.domain.Page;
import io.choerodon.core.iam.ResourceLevel;
import io.choerodon.mybatis.pagehelper.annotation.SortDefault;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;
import io.choerodon.mybatis.pagehelper.domain.Sort;
import io.choerodon.swagger.annotation.Permission;
import io.swagger.annotations.ApiOperation;
import springfox.documentation.annotations.ApiIgnore;

/**
 * 邮件通知 管理 API
 *
 * @author choerodon@choerodon.cn 2020-08-12 11:41:01
 */
@RestController(" statusNoticeSettingController.v1")
@RequestMapping("/v1/{organizationId}/ status-notice-settings")
public class StatusNoticeSettingController extends BaseController {

    @Autowired
    private StatusNoticeSettingService statusNoticeSettingService;

    @ApiOperation(value = "邮件通知列表")
    @Permission(level = ResourceLevel.ORGANIZATION)
    @GetMapping
    public ResponseEntity<Page<StatusNoticeSetting>> list(StatusNoticeSetting statusNoticeSetting,
                                                          @ApiIgnore @SortDefault(value =  StatusNoticeSetting.FIELD_ID,
            direction = Sort.Direction.DESC) PageRequest pageRequest) {
        return Results.success();
    }

    @ApiOperation(value = "创建邮件通知")
    @Permission(level = ResourceLevel.ORGANIZATION)
    @PostMapping
    public ResponseEntity<StatusNoticeSetting> create(@RequestBody StatusNoticeSetting statusNoticeSetting) {
        return Results.success(statusNoticeSetting);
    }

    @ApiOperation(value = "修改邮件通知")
    @Permission(level = ResourceLevel.ORGANIZATION)
    @PutMapping
    public ResponseEntity<StatusNoticeSetting> update(@RequestBody StatusNoticeSetting statusNoticeSetting) {
        return Results.success(statusNoticeSetting);
    }

}
