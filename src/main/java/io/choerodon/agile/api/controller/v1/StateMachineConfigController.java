package io.choerodon.agile.api.controller.v1;


import io.choerodon.core.iam.ResourceLevel;
import io.choerodon.swagger.annotation.Permission;
import io.choerodon.core.base.BaseController;
import io.choerodon.agile.api.vo.StateMachineConfigVO;
import io.choerodon.agile.app.service.StateMachineConfigService;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import org.hzero.starter.keyencrypt.core.Encrypt;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

/**
 * @author peng.jiang, dinghuang123@gmail.com
 */
@RestController
@RequestMapping(value = "/v1/organizations/{organization_id}/state_machine_config")
public class StateMachineConfigController extends BaseController {

    @Autowired
    private StateMachineConfigService configService;

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "创建配置（草稿）")
    @PostMapping(value = "/{state_machine_id}")
    public ResponseEntity<StateMachineConfigVO> create(@ApiParam(value = "组织id", required = true)
                                                       @PathVariable("organization_id") Long organizationId,
                                                       @ApiParam(value = "状态机id", required = true)
                                                       @PathVariable("state_machine_id") @Encrypt Long stateMachineId,
                                                       @ApiParam(value = "转换id", required = true)
                                                       @RequestParam("transform_id") @Encrypt Long transformId,
                                                       @ApiParam(value = "状态机配置", required = true)
                                                       @RequestBody StateMachineConfigVO configDTO) {
        return new ResponseEntity<>(configService.create(organizationId, stateMachineId, transformId, configDTO), HttpStatus.CREATED);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "删除配置（草稿）")
    @DeleteMapping(value = "/{config_id}")
    public ResponseEntity<Boolean> delete(@ApiParam(value = "组织id", required = true)
                                          @PathVariable("organization_id") Long organizationId,
                                          @ApiParam(value = "配置id", required = true)
                                          @PathVariable("config_id") @Encrypt Long configId) {
        return new ResponseEntity<>(configService.delete(organizationId, configId), HttpStatus.NO_CONTENT);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "获取转换下的配置（草稿）")
    @GetMapping(value = "/query")
    public ResponseEntity<List<StateMachineConfigVO>> queryByTransformId(@ApiParam(value = "组织id", required = true)
                                                                         @PathVariable("organization_id") Long organizationId,
                                                                         @ApiParam(value = "转换id", required = true)
                                                                         @RequestParam @Encrypt Long transformId,
                                                                         @ApiParam(value = "类型", required = true)
                                                                         @RequestParam String type) {
        return new ResponseEntity<>(configService.queryByTransformId(organizationId, transformId, type, true), HttpStatus.OK);
    }
}
