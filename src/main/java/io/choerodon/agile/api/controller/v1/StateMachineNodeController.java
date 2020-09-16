package io.choerodon.agile.api.controller.v1;


import io.choerodon.core.iam.ResourceLevel;
import io.choerodon.swagger.annotation.Permission;
import io.choerodon.core.base.BaseController;
import io.choerodon.agile.api.validator.StateMachineNodeValidator;
import io.choerodon.agile.api.vo.StatusMachineNodeVO;
import io.choerodon.agile.app.service.StateMachineNodeService;
import io.swagger.annotations.ApiOperation;
import org.hzero.starter.keyencrypt.core.Encrypt;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.Map;

/**
 * @author peng.jiang, dinghuang123@gmail.com
 */
@RestController
@RequestMapping(value = "/v1/organizations/{organization_id}/state_machine_node")
public class StateMachineNodeController extends BaseController {

    @Autowired
    private StateMachineNodeService nodeService;
    @Autowired
    private StateMachineNodeValidator nodeValidator;


    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "创建节点（草稿）")
    @PostMapping
    public ResponseEntity<List<StatusMachineNodeVO>> create(@PathVariable("organization_id") Long organizationId,
                                                            @RequestParam("stateMachineId") @Encrypt Long stateMachineId,
                                                            @RequestBody StatusMachineNodeVO nodeDTO) {
        nodeValidator.createValidate(nodeDTO);
        return new ResponseEntity<>(nodeService.create(organizationId, stateMachineId, nodeDTO), HttpStatus.CREATED);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "更新节点（草稿）")
    @PutMapping(value = "/{node_id}")
    public ResponseEntity<List<StatusMachineNodeVO>> update(@PathVariable("organization_id") Long organizationId,
                                                            @PathVariable("node_id") @Encrypt Long nodeId,
                                                            @RequestParam("stateMachineId") @Encrypt Long stateMachineId,
                                                            @RequestBody StatusMachineNodeVO nodeDTO) {
        nodeValidator.updateValidate(nodeDTO);
        return new ResponseEntity<>(nodeService.update(organizationId, stateMachineId, nodeId, nodeDTO), HttpStatus.CREATED);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "删除节点（草稿）")
    @DeleteMapping(value = "/{node_id}")
    public ResponseEntity<List<StatusMachineNodeVO>> deleteNode(@PathVariable("organization_id") Long organizationId,
                                                                @PathVariable("node_id") @Encrypt Long nodeId,
                                                                @RequestParam("stateMachineId") @Encrypt Long stateMachineId) {
        return new ResponseEntity<>(nodeService.delete(organizationId, stateMachineId, nodeId), HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "校验是否能删除节点（草稿）")
    @GetMapping(value = "/check_delete")
    public ResponseEntity<Map<String, Object>> checkDelete(@PathVariable("organization_id") Long organizationId,
                                                           @RequestParam("statusId") @Encrypt Long statusId,
                                                           @RequestParam("stateMachineId") @Encrypt Long stateMachineId) {
        return new ResponseEntity<>(nodeService.checkDelete(organizationId, stateMachineId, statusId), HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "根据id获取节点（草稿）")
    @GetMapping(value = "/{node_id}")
    public ResponseEntity<StatusMachineNodeVO> queryById(@PathVariable("organization_id") Long organizationId,
                                                         @PathVariable("node_id") @Encrypt Long nodeId) {
        return new ResponseEntity<>(nodeService.queryById(organizationId, nodeId), HttpStatus.OK);
    }

}
