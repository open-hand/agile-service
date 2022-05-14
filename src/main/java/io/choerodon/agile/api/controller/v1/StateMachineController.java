package io.choerodon.agile.api.controller.v1;


import io.choerodon.core.domain.Page;
import io.choerodon.agile.api.validator.StateMachineValidator;
import io.choerodon.agile.api.vo.StatusMachineListVO;
import io.choerodon.agile.api.vo.StatusMachineVO;
import io.choerodon.agile.app.service.StateMachineService;
import io.choerodon.agile.infra.utils.ParamUtils;
import io.choerodon.core.iam.ResourceLevel;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;
import io.choerodon.mybatis.pagehelper.domain.Sort;
import io.choerodon.swagger.annotation.Permission;
import io.choerodon.core.exception.CommonException;
import io.choerodon.mybatis.pagehelper.annotation.SortDefault;
import io.choerodon.swagger.annotation.CustomPageRequest;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import org.hzero.starter.keyencrypt.core.Encrypt;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import springfox.documentation.annotations.ApiIgnore;

import java.util.List;
import java.util.Optional;

@RestController
@RequestMapping(value = "/v1/organizations/{organization_id}/state_machine")
public class StateMachineController {

    @Autowired
    private StateMachineService stateMachineService;
    @Autowired
    private StateMachineValidator stateMachineValidator;

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "分页查询状态机列表")
    @CustomPageRequest
    @GetMapping
    public ResponseEntity<Page<StatusMachineListVO>> pagingQuery(@ApiParam(value = "组织id", required = true)
                                                                 @PathVariable("organization_id") Long organizationId,
                                                                 @ApiIgnore
                                                                 @SortDefault(value = "id", direction = Sort.Direction.DESC)
                                                                         PageRequest pageRequest,
                                                                 @ApiParam(value = "名称")
                                                                 @RequestParam(required = false) String name,
                                                                 @ApiParam(value = "描述")
                                                                 @RequestParam(required = false) String description,
                                                                 @ApiParam(value = "模糊搜索参数")
                                                                 @RequestParam(required = false) String[] param) {
        return new ResponseEntity<>(stateMachineService.pageQuery(organizationId, pageRequest, name, description, ParamUtils.arrToStr(param)), HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "创建状态机")
    @PostMapping
    public ResponseEntity<StatusMachineVO> create(@ApiParam(value = "组织id", required = true)
                                                  @PathVariable("organization_id") Long organizationId,
                                                  @ApiParam(value = "状态机", required = true)
                                                  @RequestBody StatusMachineVO statusMachineVO) {
        stateMachineValidator.createValidate(statusMachineVO);
        return new ResponseEntity<>(stateMachineService.create(organizationId, statusMachineVO), HttpStatus.CREATED);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "更新状态机")
    @PutMapping(value = "/{state_machine_id}")
    public ResponseEntity<StatusMachineVO> update(@ApiParam(value = "组织id", required = true)
                                                  @PathVariable("organization_id") Long organizationId,
                                                  @ApiParam(value = "状态机id", required = true)
                                                  @PathVariable("state_machine_id") @Encrypt Long stateMachineId,
                                                  @ApiParam(value = "状态机", required = true)
                                                  @RequestBody StatusMachineVO statusMachineVO) {
        stateMachineValidator.updateValidate(statusMachineVO);
        return new ResponseEntity<>(stateMachineService.update(organizationId, stateMachineId, statusMachineVO), HttpStatus.CREATED);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "发布状态机")
    @GetMapping(value = "/deploy/{state_machine_id}")
    public ResponseEntity<Boolean> deploy(@ApiParam(value = "组织id", required = true)
                                          @PathVariable("organization_id") Long organizationId,
                                          @ApiParam(value = "状态机id", required = true)
                                          @PathVariable("state_machine_id") @Encrypt Long stateMachineId) {
        return new ResponseEntity<>(stateMachineService.deploy(organizationId, stateMachineId, true), HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "获取状态机及配置（草稿/新建）")
    @GetMapping(value = "/with_config_draft/{state_machine_id}")
    public ResponseEntity<StatusMachineVO> queryStateMachineWithConfigDraftById(@ApiParam(value = "组织id", required = true)
                                                                                @PathVariable("organization_id") Long organizationId,
                                                                                @ApiParam(value = "状态机id", required = true)
                                                                                @PathVariable("state_machine_id") @Encrypt Long stateMachineId) {
        return new ResponseEntity<>(stateMachineService.queryStateMachineWithConfigById(organizationId, stateMachineId, true), HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "获取状态机原件及配置（活跃）")
    @GetMapping(value = "/with_config_deploy/{state_machine_id}")
    public ResponseEntity<StatusMachineVO> queryStateMachineWithConfigOriginById(@ApiParam(value = "组织id", required = true)
                                                                                 @PathVariable("organization_id") Long organizationId,
                                                                                 @ApiParam(value = "状态机id", required = true)
                                                                                 @PathVariable("state_machine_id") @Encrypt Long stateMachineId) {

        return new ResponseEntity<>(stateMachineService.queryStateMachineWithConfigById(organizationId, stateMachineId, false), HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "获取状态机（无配置）")
    @GetMapping(value = "/{state_machine_id}")
    public ResponseEntity<StatusMachineVO> queryStateMachineById(@ApiParam(value = "组织id", required = true)
                                                                 @PathVariable("organization_id") Long organizationId,
                                                                 @ApiParam(value = "状态机id", required = true)
                                                                 @PathVariable("state_machine_id") @Encrypt Long stateMachineId) {
        return new ResponseEntity<>(stateMachineService.queryStateMachineById(organizationId, stateMachineId), HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "删除草稿")
    @DeleteMapping(value = "/delete_draft/{state_machine_id}")
    public ResponseEntity<StatusMachineVO> deleteDraft(@ApiParam(value = "组织id", required = true)
                                                       @PathVariable("organization_id") Long organizationId,
                                                       @ApiParam(value = "状态机id", required = true)
                                                       @PathVariable("state_machine_id") @Encrypt Long stateMachineId) {
        return new ResponseEntity<>(stateMachineService.deleteDraft(organizationId, stateMachineId), HttpStatus.NO_CONTENT);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "删除状态机")
    @DeleteMapping(value = "/{state_machine_id}")
    public ResponseEntity delete(@ApiParam(value = "组织id", required = true)
                                 @PathVariable("organization_id") Long organizationId,
                                 @ApiParam(value = "状态机id", required = true)
                                 @PathVariable("state_machine_id") @Encrypt Long stateMachineId) {
        stateMachineService.delete(organizationId, stateMachineId);
        return new ResponseEntity(HttpStatus.NO_CONTENT);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "校验状态机名字是否未被使用")
    @GetMapping(value = "/check_name")
    public ResponseEntity<Boolean> checkName(@ApiParam(value = "组织id", required = true)
                                             @PathVariable("organization_id") Long organizationId,
                                             @ApiParam(value = "名称", required = true)
                                             @RequestParam("name") String name) {
        return Optional.ofNullable(stateMachineService.checkName(organizationId, name))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.stateMachineName.check"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "获取组织下所有状态机")
    @GetMapping(value = "/query_all")
    public ResponseEntity<List<StatusMachineVO>> queryAll(@ApiParam(value = "组织id", required = true)
                                                          @PathVariable("organization_id") Long organizationId) {
        return new ResponseEntity<>(stateMachineService.queryAll(organizationId), HttpStatus.OK);
    }
}
