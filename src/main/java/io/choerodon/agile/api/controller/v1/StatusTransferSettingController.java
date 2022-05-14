package io.choerodon.agile.api.controller.v1;

import io.choerodon.agile.api.vo.StatusTransferSettingCreateVO;
import io.choerodon.agile.app.service.StatusTransferSettingService;
import io.choerodon.agile.infra.dto.StatusDTO;
import io.choerodon.agile.infra.dto.StatusTransferSettingDTO;
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

/**
 * @author zhaotianxin
 * @date 2020-08-12 14:56
 */
@RestController
@RequestMapping(value = "/v1/projects/{project_id}/status_transfer_setting")
public class StatusTransferSettingController {
    @Autowired
    private StatusTransferSettingService statusTransferSettingService;

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "为问题类型的状态创建流转条件")
    @PostMapping
    public ResponseEntity createOrUpdate(@ApiParam(value = "项目id", required = true)
                                         @PathVariable("project_id") Long projectId,
                                         @ApiParam(value = "问题类型id", required = true)
                                         @RequestParam @Encrypt Long issueTypeId,
                                         @ApiParam(value = "状态id", required = true)
                                         @RequestParam @Encrypt Long statusId,
                                         @ApiParam(value = "乐观锁", required = true)
                                         @RequestParam Long objectVersionNumber,
                                         @ApiParam(value = "应用类型", required = true)
                                         @RequestParam String applyType,
                                         @ApiParam(value = "状态转换配置", required = true)
                                         @RequestBody List<StatusTransferSettingCreateVO> list) {
        statusTransferSettingService.createOrUpdate(projectId, issueTypeId, statusId, objectVersionNumber, applyType, list);
        return new ResponseEntity(HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "查询流转条件")
    @GetMapping(value = "/query_transfer")
    public ResponseEntity<List<StatusTransferSettingDTO>> queryStatusTransferSetting(@ApiParam(value = "项目id", required = true)
                                                                                     @PathVariable("project_id") Long projectId,
                                                                                     @ApiParam(value = "问题类型id", required = true)
                                                                                     @RequestParam @Encrypt Long issueTypeId,
                                                                                     @ApiParam(value = "状态id", required = true)
                                                                                     @RequestParam @Encrypt Long statusId) {
        return new ResponseEntity(statusTransferSettingService.query(projectId, issueTypeId, statusId), HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "查询当前issue不能流转到的状态")
    @GetMapping(value = "/not_allowed_transfer")
    public ResponseEntity<List<StatusDTO>> queryNotAllowedTransferStatus(@ApiParam(value = "项目id", required = true)
                                                                         @PathVariable("project_id") Long projectId,
                                                                         @ApiParam(value = "问题id", required = true)
                                                                         @RequestParam @Encrypt Long issueId) {
        return new ResponseEntity<>(statusTransferSettingService.queryNotAllowedTransferStatus(projectId, issueId), HttpStatus.OK);
    }
}
