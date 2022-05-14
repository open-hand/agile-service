package io.choerodon.agile.api.controller.v1;


import io.choerodon.core.iam.ResourceLevel;
import io.choerodon.swagger.annotation.Permission;
import io.choerodon.core.base.BaseController;
import io.choerodon.agile.infra.statemachineclient.dto.ExecuteResult;
import io.choerodon.agile.api.vo.StatusMachineTransformVO;
import io.choerodon.agile.api.vo.event.TransformInfo;
import io.choerodon.agile.app.service.InstanceService;
import io.choerodon.agile.infra.cache.InstanceCache;
import io.choerodon.agile.infra.statemachineclient.dto.InputDTO;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import org.hzero.starter.keyencrypt.core.Encrypt;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.Map;

/**
 * @author shinan.chen
 * @date 2018/9/17
 */
@RestController
@RequestMapping(value = "/v1/organizations/{organization_id}/instances")
public class InstanceController extends BaseController {

    @Autowired
    private InstanceService instanceService;
    @Autowired
    private InstanceCache instanceCache;

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "创建状态机实例")
    @PostMapping(value = "/start_instance")
    public ResponseEntity<ExecuteResult> startInstance(@ApiParam(value = "组织id", required = true)
                                                           @PathVariable("organization_id") Long organizationId,
                                                       @ApiParam(value = "服务编码", required = true)
                                                       @RequestParam("service_code") String serviceCode,
                                                       @ApiParam(value = "状态机id", required = true)
                                                       @RequestParam("state_machine_id") @Encrypt Long stateMachineId,
                                                       @ApiParam(value = "输入参数", required = true)
                                                       @RequestBody InputDTO inputDTO) {
        ExecuteResult result = instanceService.startInstance(organizationId, serviceCode, stateMachineId, inputDTO);
        return new ResponseEntity<>(result, HttpStatus.CREATED);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "执行状态转换，并返回转换后的状态")
    @PostMapping(value = "/execute_transform")
    public ResponseEntity<ExecuteResult> executeTransform(@ApiParam(value = "组织ID", required = true)
                                                          @PathVariable("organization_id") Long organizationId,
                                                          @ApiParam(value = "服务编码", required = true)
                                                          @RequestParam("service_code") String serviceCode,
                                                          @ApiParam(value = "状态机id", required = true)
                                                          @RequestParam("state_machine_id") @Encrypt Long stateMachineId,
                                                          @ApiParam(value = "当前状态机id", required = true)
                                                          @RequestParam("current_status_id") @Encrypt Long currentStatusId,
                                                          @ApiParam(value = "转换id", required = true)
                                                          @RequestParam("transform_id") @Encrypt Long transformId,
                                                          @RequestBody InputDTO inputDTO) {
        ExecuteResult result = instanceService.executeTransform(organizationId, serviceCode, stateMachineId, currentStatusId, transformId, inputDTO);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "获取当前状态拥有的转换列表，feign调用对应服务的条件验证")
    @GetMapping(value = "/transform_list")
    public ResponseEntity<List<TransformInfo>> queryListTransform(@ApiParam(value = "组织id", required = true)
                                                                  @PathVariable("organization_id") Long organizationId,
                                                                  @ApiParam(value = "服务编码", required = true)
                                                                  @RequestParam("service_code") String serviceCode,
                                                                  @ApiParam(value = "状态机id", required = true)
                                                                  @RequestParam("state_machine_id") @Encrypt Long stateMachineId,
                                                                  @ApiParam(value = "实例id", required = true)
                                                                  @RequestParam("instance_id") @Encrypt Long instanceId,
                                                                  @ApiParam(value = "当前状态机id", required = true)
                                                                  @RequestParam("current_status_id") @Encrypt Long currentStateId) {
        return new ResponseEntity<>(instanceService.queryListTransform(organizationId, serviceCode, stateMachineId, instanceId, currentStateId), HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "获取状态机的初始状态")
    @GetMapping(value = "/query_init_status_id")
    public ResponseEntity<Long> queryInitStatusId(@ApiParam(value = "组织id", required = true)
                                                  @PathVariable("organization_id") Long organizationId,
                                                  @ApiParam(value = "状态机id", required = true)
                                                  @RequestParam("state_machine_id") @Encrypt Long stateMachineId) {
        return new ResponseEntity<>(instanceService.queryInitStatusId(organizationId, stateMachineId), HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "获取状态机对应的初始状态Map")
    @GetMapping(value = "/query_init_status_ids")
    public ResponseEntity<Map<Long, Long>> queryInitStatusIds(@ApiParam(value = "组织id", required = true)
                                                              @PathVariable("organization_id") Long organizationId,
                                                              @ApiParam(value = "状态机id集合", required = true)
                                                              @RequestParam("state_machine_id") List<Long> stateMachineIds) {
        return new ResponseEntity<>(instanceService.queryInitStatusIds(organizationId, stateMachineIds), HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "创建实例时，获取状态机的初始转换，包括转换的configs")
    @GetMapping(value = "/query_init_transform")
    public ResponseEntity<StatusMachineTransformVO> queryInitTransform(@ApiParam(value = "组织id", required = true)
                                                                       @PathVariable("organization_id") Long organizationId,
                                                                       @ApiParam(value = "状态机id", required = true)
                                                                       @RequestParam("state_machine_id") @Encrypt Long stateMachineId) {
        return new ResponseEntity<>(instanceService.queryInitTransform(organizationId, stateMachineId), HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "手动清理状态机实例")
    @GetMapping(value = "/cleanInstance")
    @Transactional(rollbackFor = Exception.class)
    public void cleanInstance(@ApiParam(value = "组织id", required = true)
                              @PathVariable("organization_id") Long organizationId,
                              @ApiParam(value = "是否清理状态机实例", required = true)
                              @RequestParam("is_clean_all") Boolean isCleanAll) {
        if (Boolean.TRUE.equals(isCleanAll)) {
            instanceCache.cleanAllInstances();
        } else {
            instanceCache.cleanInstanceTask();
        }
    }
}