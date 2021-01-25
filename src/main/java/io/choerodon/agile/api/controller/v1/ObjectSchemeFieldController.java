package io.choerodon.agile.api.controller.v1;


import io.choerodon.agile.api.vo.*;
import io.choerodon.core.iam.ResourceLevel;
import io.choerodon.swagger.annotation.Permission;
import io.choerodon.agile.app.service.ObjectSchemeFieldService;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import org.hzero.starter.keyencrypt.core.Encrypt;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import java.util.List;
import java.util.Map;

/**
 * @author shinan.chen
 * @since 2019/3/29
 */
@RestController
@RequestMapping("/v1/organizations/{organization_id}/object_scheme_field")
public class ObjectSchemeFieldController {

    @Autowired
    private ObjectSchemeFieldService objectSchemeFieldService;

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "根据方案编码获取字段列表")
    @GetMapping("/list")
    public ResponseEntity<Map<String, Object>> listQuery(@ApiParam(value = "组织id", required = true)
                                                         @PathVariable("organization_id") Long organizationId,
                                                         @ApiParam(value = "方案编码", required = true)
                                                         @RequestParam String schemeCode) {
        return new ResponseEntity<>(objectSchemeFieldService.listQuery(organizationId, null, schemeCode), HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "创建字段")
    @PostMapping
    public ResponseEntity<ObjectSchemeFieldDetailVO> create(@ApiParam(value = "组织id", required = true)
                                                             @PathVariable("organization_id") Long organizationId,
                                                            @ApiParam(value = "字段对象", required = true)
                                                             @RequestBody @Valid  ObjectSchemeFieldCreateVO fieldCreateDTO) {
        return new ResponseEntity<>(objectSchemeFieldService.create(organizationId, null, fieldCreateDTO, null), HttpStatus.CREATED);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "查询字段详情")
    @GetMapping(value = "/{field_id}")
    public ResponseEntity<ObjectSchemeFieldDetailVO> queryById(@ApiParam(value = "组织id", required = true)
                                                                @PathVariable("organization_id") Long organizationId,
                                                               @ApiParam(value = "字段id", required = true)
                                                                @PathVariable("field_id") @Encrypt Long fieldId) {
        return new ResponseEntity<>(objectSchemeFieldService.queryById(organizationId, null, fieldId), HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "删除字段")
    @DeleteMapping(value = "/{field_id}")
    public ResponseEntity delete(@ApiParam(value = "组织id", required = true)
                                 @PathVariable("organization_id") Long organizationId,
                                 @PathVariable("field_id") @Encrypt Long fieldId) {
        objectSchemeFieldService.delete(organizationId, null, fieldId);
        return new ResponseEntity<>(HttpStatus.NO_CONTENT);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "修改字段")
    @PutMapping(value = "/{field_id}")
    public ResponseEntity<ObjectSchemeFieldDetailVO> update(@ApiParam(value = "组织id", required = true)
                                                             @PathVariable("organization_id") Long organizationId,
                                                            @ApiParam(value = "字段id", required = true)
                                                             @PathVariable("field_id") @Encrypt Long fieldId,
                                                            @RequestBody @Valid  ObjectSchemeFieldUpdateVO updateDTO) {
        return new ResponseEntity<>(objectSchemeFieldService.update(organizationId, null, fieldId, updateDTO), HttpStatus.CREATED);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "校验字段名称是否已使用")
    @GetMapping(value = "/check_name")
    public ResponseEntity<Boolean> checkName(@ApiParam(value = "组织id", required = true)
                                             @PathVariable("organization_id") Long organizationId,
                                             @ApiParam(value = "字段名称", required = true)
                                             @RequestParam("name") String name,
                                             @ApiParam(value = "方案编码", required = true)
                                             @RequestParam String schemeCode) {
        return new ResponseEntity<>(objectSchemeFieldService.checkName(organizationId, null, name, schemeCode), HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "校验字段编码是否已使用")
    @GetMapping(value = "/check_code")
    public ResponseEntity<Boolean> checkCode(@ApiParam(value = "组织id", required = true)
                                             @PathVariable("organization_id") Long organizationId,
                                             @ApiParam(value = "字段编码", required = true)
                                             @RequestParam("code") String code,
                                             @ApiParam(value = "方案编码", required = true)
                                             @RequestParam String schemeCode) {
        return new ResponseEntity<>(objectSchemeFieldService.checkCode(organizationId, null, code, schemeCode), HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "页面配置接口")
    @PostMapping(value = "/configs")
    public ResponseEntity config(@ApiParam(value = "组织id", required = true)
                                 @PathVariable("organization_id") Long organizationId,
                                 @RequestBody PageConfigUpdateVO pageConfigUpdateVO) {
        objectSchemeFieldService.config(organizationId, null, pageConfigUpdateVO);
        return new ResponseEntity<>(HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "查询字段的页面配置数据")
    @GetMapping(value = "/configs")
    public ResponseEntity<PageConfigVO> listConfigs(@PathVariable("organization_id") Long organizationId,
                                                    @RequestParam @Encrypt Long issueTypeId) {
        return new ResponseEntity<>(objectSchemeFieldService.listConfigs(organizationId, null, issueTypeId), HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "更新字段是否必填")
    @PostMapping(value = "/update_required")
    public ResponseEntity updateRequired(@PathVariable("organization_id") Long organizationId,
                                         @RequestParam @Encrypt Long fieldId,
                                         @RequestParam Boolean required) {
        objectSchemeFieldService.updateRequired(organizationId, null, fieldId, required);
        return new ResponseEntity<>(HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "拖动查询rank值接口")
    @PostMapping(value = "/rank")
    public ResponseEntity<String> queryRank(@PathVariable("organization_id") Long organizationId,
                                            @RequestBody AdjustOrderVO adjustOrderVO) {
        return new ResponseEntity<>(objectSchemeFieldService.queryRank(adjustOrderVO.getPreviousRank(), adjustOrderVO.getNextRank()), HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "页面配置根据问题类型查询未选择的字段")
    @GetMapping(value = "/unselected")
    public ResponseEntity<List<ObjectSchemeFieldVO>> unselected(@PathVariable("organization_id") Long organizationId,
                                                                @RequestParam @Encrypt Long issueTypeId) {
        return new ResponseEntity<>(objectSchemeFieldService.unselected(organizationId, null, issueTypeId), HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "页面配置类型查询接口")
    @GetMapping(value = "/configs/issue_types")
    public ResponseEntity<List<IssueTypeVO>> issueTypes(@PathVariable("organization_id") Long organizationId) {
        return new ResponseEntity<>(objectSchemeFieldService.issueTypes(organizationId, null), HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "同步字段默认值到扩展字段类型")
    @PostMapping(value = "/sync_default_value")
    public ResponseEntity syncDefaultValue(@ApiParam(value = "组织id", required = true)
                                           @PathVariable("organization_id") Long organizationId,
                                           @ApiParam(value = "字段id", required = true)
                                           @RequestParam("field_id") @Encrypt Long fieldId,
                                           @ApiParam(value = "问题类型", required = true)
                                           @RequestParam("context") String context,
                                           @RequestBody @Valid  ObjectSchemeFieldUpdateVO updateDTO) {
        objectSchemeFieldService.syncDefaultValue(organizationId, null, fieldId, context, updateDTO);
        return new ResponseEntity<>(HttpStatus.CREATED);
    }

}
