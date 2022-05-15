package io.choerodon.agile.api.controller.v1;


import io.choerodon.core.domain.Page;
import io.choerodon.core.iam.ResourceLevel;
import io.choerodon.mybatis.pagehelper.domain.Sort;
import io.choerodon.swagger.annotation.Permission;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;
import io.choerodon.core.exception.CommonException;
import io.choerodon.agile.api.validator.StateMachineSchemeValidator;
import io.choerodon.agile.api.vo.StatusMachineSchemeConfigVO;
import io.choerodon.agile.api.vo.StateMachineSchemeVO;
import io.choerodon.agile.api.vo.event.StateMachineSchemeChangeItem;
import io.choerodon.agile.app.service.StateMachineSchemeConfigService;
import io.choerodon.agile.app.service.StateMachineSchemeService;
import io.choerodon.agile.infra.utils.ParamUtils;
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
@RequestMapping(value = "/v1/organizations/{organization_id}/state_machine_scheme")
public class StateMachineSchemeController {

    @Autowired
    private StateMachineSchemeService schemeService;
    @Autowired
    private StateMachineSchemeValidator schemeValidator;
    @Autowired
    private StateMachineSchemeConfigService configService;

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "查询状态机方案列表")
    @CustomPageRequest
    @GetMapping
    public ResponseEntity<Page<StateMachineSchemeVO>> pagingQuery(@ApiIgnore
                                                                  @SortDefault(value = "id", direction = Sort.Direction.DESC) PageRequest pageRequest,
                                                                  @ApiParam(value = "组织id", required = true)
                                                                  @PathVariable("organization_id") Long organizationId,
                                                                  @ApiParam(value = "名称")
                                                                  @RequestParam(required = false) String name,
                                                                  @ApiParam(value = "描述")
                                                                  @RequestParam(required = false) String description,
                                                                  @ApiParam(value = "模糊查询参数")
                                                                  @RequestParam(required = false) String[] param) {
        StateMachineSchemeVO schemeDTO = new StateMachineSchemeVO();
        schemeDTO.setOrganizationId(organizationId);
        schemeDTO.setName(name);
        schemeDTO.setDescription(description);
        return new ResponseEntity<>(schemeService.pageQuery(organizationId, pageRequest, schemeDTO, ParamUtils.arrToStr(param)), HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "创建状态机方案")
    @PostMapping
    public ResponseEntity<StateMachineSchemeVO> create(@ApiParam(value = "组织id", required = true)
                                                       @PathVariable("organization_id") Long organizationId,
                                                       @ApiParam(value = "状态机方案", required = true)
                                                       @RequestBody StateMachineSchemeVO schemeDTO) {
        schemeValidator.createValidate(schemeDTO);
        return new ResponseEntity<>(schemeService.create(organizationId, schemeDTO), HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "更新状态机方案")
    @PutMapping(value = "/{scheme_id}")
    public ResponseEntity<StateMachineSchemeVO> update(@ApiParam(value = "组织id", required = true)
                                                       @PathVariable("organization_id") Long organizationId,
                                                       @ApiParam(value = "方案id", required = true)
                                                       @PathVariable("scheme_id") @Encrypt Long schemeId,
                                                       @ApiParam(value = "方案", required = true)
                                                       @RequestBody StateMachineSchemeVO schemeDTO) {
        schemeValidator.updateValidate(schemeDTO);
        return new ResponseEntity<>(schemeService.update(organizationId, schemeId, schemeDTO), HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "删除状态机方案")
    @DeleteMapping(value = "/{scheme_id}")
    public ResponseEntity<Boolean> delete(@ApiParam(value = "组织id", required = true)
                                          @PathVariable("organization_id") Long organizationId,
                                          @ApiParam(value = "方案id", required = true)
                                          @PathVariable("scheme_id") @Encrypt Long schemeId) {
        return new ResponseEntity<>(schemeService.delete(organizationId, schemeId), HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "根据id查询状态机方案对象")
    @GetMapping(value = "/query_scheme_with_config/{scheme_id}")
    public ResponseEntity<StateMachineSchemeVO> querySchemeWithConfigById(@ApiParam(value = "组织id", required = true)
                                                                          @PathVariable("organization_id") Long organizationId,
                                                                          @ApiParam(value = "方案id", required = true)
                                                                          @PathVariable("scheme_id") @Encrypt Long schemeId,
                                                                          @ApiParam(value = "是否为草稿", required = true)
                                                                          @RequestParam("isDraft") Boolean isDraft) {
        return new ResponseEntity<>(schemeService.querySchemeWithConfigById(isDraft, organizationId, schemeId, null), HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "创建方案配置")
    @PostMapping(value = "/create_config/{scheme_id}/{state_machine_id}")
    public ResponseEntity<StateMachineSchemeVO> createConfig(@ApiParam(value = "组织id", required = true)
                                                             @PathVariable("organization_id") Long organizationId,
                                                             @ApiParam(value = "方案id", required = true)
                                                             @PathVariable("scheme_id") @Encrypt Long schemeId,
                                                             @ApiParam(value = "状态机id", required = true)
                                                             @PathVariable("state_machine_id") @Encrypt Long stateMachineId,
                                                             @ApiParam(value = "状态机方案配置", required = true)
                                                             @RequestBody List<StatusMachineSchemeConfigVO> schemeDTOs) {
        return new ResponseEntity<>(configService.create(organizationId, schemeId, stateMachineId, schemeDTOs), HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "根据状态机id删除方案配置")
    @DeleteMapping(value = "/delete_config/{scheme_id}/{state_machine_id}")
    public ResponseEntity<StateMachineSchemeVO> deleteConfig(@ApiParam(value = "组织id", required = true)
                                                             @PathVariable("organization_id") Long organizationId,
                                                             @ApiParam(value = "方案id", required = true)
                                                             @PathVariable("scheme_id") @Encrypt Long schemeId,
                                                             @ApiParam(value = "状态机id", required = true)
                                                             @PathVariable("state_machine_id") @Encrypt Long stateMachineId) {
        return new ResponseEntity<>(configService.delete(organizationId, schemeId, stateMachineId), HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "校验状态机方案名字是否未被使用")
    @GetMapping(value = "/check_name")
    public ResponseEntity<Boolean> checkName(@ApiParam(value = "组织id", required = true)
                                             @PathVariable("organization_id") Long organizationId,
                                             @ApiParam(value = "名称", required = true)
                                             @RequestParam("name") String name) {
        return Optional.ofNullable(schemeService.checkName(organizationId, name))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.stateMachineSchemeName.check"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "校验发布状态机方案")
    @GetMapping(value = "/check_deploy/{scheme_id}")
    public ResponseEntity<List<StateMachineSchemeChangeItem>> checkDeploy(@ApiParam(value = "组织id", required = true)
                                                                          @PathVariable("organization_id") Long organizationId,
                                                                          @ApiParam(value = "方案id", required = true)
                                                                          @PathVariable("scheme_id") @Encrypt Long schemeId) {
        return new ResponseEntity<>(configService.checkDeploy(organizationId, schemeId), HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "发布状态机方案")
    @PostMapping(value = "/deploy/{scheme_id}")
    public ResponseEntity<Boolean> deploy(@ApiParam(value = "组织id", required = true)
                                          @PathVariable("organization_id") Long organizationId,
                                          @ApiParam(value = "方案id", required = true)
                                          @PathVariable("scheme_id") @Encrypt Long schemeId,
                                          @ApiParam(value = "乐观锁", required = true)
                                          @RequestParam("objectVersionNumber") Long objectVersionNumber,
                                          @ApiParam(value = "状态机方案改变项", required = true)
                                          @RequestBody List<StateMachineSchemeChangeItem> changeItems) {
        return new ResponseEntity<>(configService.deploy(organizationId, schemeId, changeItems, objectVersionNumber), HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "删除状态机方案草稿")
    @DeleteMapping(value = "/delete_draft/{scheme_id}")
    public ResponseEntity<StateMachineSchemeVO> deleteDraft(@ApiParam(value = "组织id", required = true)
                                                            @PathVariable("organization_id") Long organizationId,
                                                            @ApiParam(value = "方案id", required = true)
                                                            @PathVariable("scheme_id") @Encrypt Long stateMachineId) {
        return new ResponseEntity<>(configService.deleteDraft(organizationId, stateMachineId), HttpStatus.NO_CONTENT);
    }
}
