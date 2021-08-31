package io.choerodon.agile.api.controller.v1;

import io.choerodon.agile.api.vo.StatusTransferSettingCreateVO;
import io.choerodon.agile.app.service.StatusTransferSettingService;
import io.choerodon.agile.infra.dto.StatusTransferSettingDTO;
import io.choerodon.core.iam.ResourceLevel;
import io.choerodon.swagger.annotation.Permission;
import io.swagger.annotations.ApiOperation;
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
    public ResponseEntity createOrUpdate(@PathVariable("project_id") Long projectId,
                                          @RequestParam @Encrypt Long issueTypeId,
                                          @RequestParam @Encrypt Long statusId,
                                          @RequestParam Long objectVersionNumber,
                                          @RequestParam String applyType,
                                          @RequestBody List<StatusTransferSettingCreateVO>list) {
        statusTransferSettingService.createOrUpdate(projectId,issueTypeId,statusId,objectVersionNumber,applyType,list);
        return new ResponseEntity(HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "查询流转条件")
    @GetMapping(value = "/query_transfer")
    public ResponseEntity<List<StatusTransferSettingDTO>> queryStatusTransferSetting(@PathVariable("project_id") Long projectId,
                                                                                     @RequestParam @Encrypt Long issueTypeId,
                                                                                     @RequestParam @Encrypt Long statusId) {
        return new ResponseEntity(statusTransferSettingService.query(projectId,issueTypeId,statusId),HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "校验能否流转到目标状态")
    @GetMapping(value = "/check_transfer")
    public ResponseEntity<Boolean> checkTransfer(@PathVariable("project_id") Long projectId,
                                                        @RequestParam @Encrypt Long issueId,
                                                        @RequestParam @Encrypt Long targetStatusId){
        return new ResponseEntity<>(statusTransferSettingService.checkTransfer(projectId, issueId, targetStatusId),HttpStatus.OK);
    }
}
