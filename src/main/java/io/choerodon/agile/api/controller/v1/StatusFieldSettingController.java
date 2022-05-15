package io.choerodon.agile.api.controller.v1;

import io.choerodon.agile.api.vo.StatusFieldSettingVO;
import io.choerodon.agile.app.service.StatusFieldSettingService;
import io.choerodon.core.exception.CommonException;
import io.choerodon.core.iam.ResourceLevel;
import io.choerodon.swagger.annotation.Permission;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import org.hzero.starter.keyencrypt.core.Encrypt;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.Optional;

/**
 * @author zhaotianxin
 * @date 2020-08-13 17:11
 */
@RestController
@RequestMapping("/v1/projects/{project_id}/status_field_settings")
public class StatusFieldSettingController {
    @Autowired
    private StatusFieldSettingService statusFieldSettingService;

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("为问题类型配置状态流转后的更改属性")
    @PostMapping
    public ResponseEntity<List<StatusFieldSettingVO>> createOrUpdate(@ApiParam(value = "项目id", required = true)
                                                                     @PathVariable(name = "project_id") Long projectId,
                                                                     @ApiParam(value = "问题类型id", required = true)
                                                                     @RequestParam @Encrypt Long issueTypeId,
                                                                     @ApiParam(value = "状态id", required = true)
                                                                     @RequestParam @Encrypt Long statusId,
                                                                     @ApiParam(value = "乐观锁", required = true)
                                                                     @RequestParam Long objectVersionNumber,
                                                                     @ApiParam(value = "应用类型", required = true)
                                                                     @RequestParam String applyType,
                                                                     @ApiParam(value = "状态流转字段配置", required = true)
                                                                     @RequestBody List<StatusFieldSettingVO> list) {
        return Optional.ofNullable(statusFieldSettingService.createOrUpdate(projectId, issueTypeId, statusId, objectVersionNumber, applyType, list))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.status.field.setting.get"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("查询")
    @GetMapping("/list")
    public ResponseEntity<List<StatusFieldSettingVO>> listFieldSetting(@ApiParam(value = "项目id", required = true)
                                                                       @PathVariable(name = "project_id") Long projectId,
                                                                       @ApiParam(value = "问题类型id", required = true)
                                                                       @RequestParam @Encrypt Long issueTypeId,
                                                                       @ApiParam(value = "状态id", required = true)
                                                                       @RequestParam @Encrypt Long statusId) {
        return Optional.ofNullable(statusFieldSettingService.list(projectId, issueTypeId, statusId))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.status.field.setting.get"));
    }
}
