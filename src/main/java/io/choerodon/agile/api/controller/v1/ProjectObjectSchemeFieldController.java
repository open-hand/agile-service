package io.choerodon.agile.api.controller.v1;


import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.app.service.FieldOptionService;
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
@RequestMapping("/v1/projects/{project_id}/object_scheme_field")
public class ProjectObjectSchemeFieldController {

    @Autowired
    private ObjectSchemeFieldService objectSchemeFieldService;
    @Autowired
    private FieldOptionService fieldOptionService;

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "根据方案编码获取字段列表")
    @GetMapping("/list")
    public ResponseEntity<Map<String, Object>> listQuery(@ApiParam(value = "项目id", required = true)
                                                         @PathVariable("project_id") Long projectId,
                                                         @ApiParam(value = "组织id", required = true)
                                                         @RequestParam Long organizationId,
                                                         @ApiParam(value = "方案编码", required = true)
                                                         @RequestParam String schemeCode) {
        return new ResponseEntity<>(objectSchemeFieldService.listQuery(organizationId, projectId, schemeCode), HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "创建字段")
    @PostMapping
    public ResponseEntity<ObjectSchemeFieldDetailVO> create(@ApiParam(value = "项目id", required = true)
                                                             @PathVariable("project_id") Long projectId,
                                                            @ApiParam(value = "组织id", required = true)
                                                             @RequestParam Long organizationId,
                                                            @ApiParam(value = "字段对象", required = true)
                                                             @RequestBody @Valid ObjectSchemeFieldCreateVO fieldCreateDTO) {
        return new ResponseEntity<>(objectSchemeFieldService.create(organizationId, projectId, fieldCreateDTO, null), HttpStatus.CREATED);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "查询字段详情")
    @GetMapping(value = "/{field_id}")
    public ResponseEntity<ObjectSchemeFieldDetailVO> queryById(@ApiParam(value = "项目id", required = true)
                                                                @PathVariable("project_id") Long projectId,
                                                               @ApiParam(value = "组织id", required = true)
                                                                @RequestParam Long organizationId,
                                                               @ApiParam(value = "字段id", required = true)
                                                                @PathVariable("field_id") @Encrypt Long fieldId) {
        return new ResponseEntity<>(objectSchemeFieldService.queryById(organizationId, projectId, fieldId), HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "删除字段")
    @DeleteMapping(value = "/{field_id}")
    public ResponseEntity delete(@ApiParam(value = "项目id", required = true)
                                 @PathVariable("project_id") Long projectId,
                                 @ApiParam(value = "组织id", required = true)
                                 @RequestParam Long organizationId,
                                 @ApiParam(value = "字段id", required = true)
                                 @PathVariable("field_id") @Encrypt Long fieldId) {
        objectSchemeFieldService.delete(organizationId, projectId, fieldId);
        return new ResponseEntity<>(HttpStatus.NO_CONTENT);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "修改字段")
    @PutMapping(value = "/{field_id}")
    public ResponseEntity<ObjectSchemeFieldDetailVO> update(@ApiParam(value = "项目id", required = true)
                                                             @PathVariable("project_id") Long projectId,
                                                            @ApiParam(value = "组织id", required = true)
                                                             @RequestParam Long organizationId,
                                                            @ApiParam(value = "字段id", required = true)
                                                             @PathVariable("field_id") @Encrypt Long fieldId,
                                                            @ApiParam(value = "字段对象", required = true)
                                                            @RequestBody @Valid  ObjectSchemeFieldUpdateVO updateDTO) {
        return new ResponseEntity<>(objectSchemeFieldService.update(organizationId, projectId, fieldId, updateDTO), HttpStatus.CREATED);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "校验字段名称是否已使用")
    @GetMapping(value = "/check_name")
    public ResponseEntity<Boolean> checkName(@ApiParam(value = "项目id", required = true)
                                             @PathVariable("project_id") Long projectId,
                                             @ApiParam(value = "组织id", required = true)
                                             @RequestParam Long organizationId,
                                             @ApiParam(value = "字段名称", required = true)
                                             @RequestParam("name") String name,
                                             @ApiParam(value = "方案编码", required = true)
                                             @RequestParam String schemeCode) {
        return new ResponseEntity<>(objectSchemeFieldService.checkName(organizationId, projectId, name, schemeCode), HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "校验字段编码是否已使用")
    @GetMapping(value = "/check_code")
    public ResponseEntity<Boolean> checkCode(@ApiParam(value = "项目id", required = true)
                                             @PathVariable("project_id") Long projectId,
                                             @ApiParam(value = "组织id", required = true)
                                             @RequestParam Long organizationId,
                                             @ApiParam(value = "字段编码", required = true)
                                             @RequestParam("code") String code,
                                             @ApiParam(value = "方案编码", required = true)
                                             @RequestParam String schemeCode) {
        return new ResponseEntity<>(objectSchemeFieldService.checkCode(organizationId, projectId, code, schemeCode), HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "查询字段的页面配置数据")
    @GetMapping(value = "/configs")
    public ResponseEntity<PageConfigVO> listConfigs(@ApiParam(value = "项目id", required = true)
                                                    @PathVariable(name = "project_id") Long projectId,
                                                    @ApiParam(value = "组织id", required = true)
                                                    @RequestParam Long organizationId,
                                                    @ApiParam(value = "问题类型id", required = true)
                                                    @RequestParam @Encrypt Long issueTypeId) {
        return new ResponseEntity<>(objectSchemeFieldService.listConfigs(organizationId, projectId, issueTypeId), HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "页面配置接口")
    @PostMapping(value = "/configs")
    public ResponseEntity<Boolean> config(@ApiParam(value = "项目id", required = true)
                                          @PathVariable(name = "project_id") Long projectId,
                                          @ApiParam(value = "组织id", required = true)
                                          @RequestParam Long organizationId,
                                          @ApiParam(value = "页面配置对象", required = true)
                                          @RequestBody PageConfigUpdateVO pageConfigUpdateVO) {
        objectSchemeFieldService.config(organizationId, projectId, pageConfigUpdateVO);
        return new ResponseEntity<>(HttpStatus.OK);
    }


    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "更新字段是否必填")
    @PostMapping(value = "/update_required")
    public ResponseEntity updateRequired(@ApiParam(value = "项目id", required = true)
                                         @PathVariable("project_id") Long projectId,
                                         @ApiParam(value = "字段id", required = true)
                                         @RequestParam @Encrypt Long fieldId,
                                         @ApiParam(value = "组织id", required = true)
                                         @RequestParam Long organizationId,
                                         @ApiParam(value = "是否为必填", required = true)
                                         @RequestParam Boolean required) {
        objectSchemeFieldService.updateRequired(organizationId, projectId, fieldId, required);
        return new ResponseEntity<>(HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "拖动查询rank值接口")
    @PostMapping(value = "/rank")
    public ResponseEntity<String> queryRank(@ApiParam(value = "项目id", required = true)
                                            @PathVariable("project_id") Long projectId,
                                            @ApiParam(value = "组织id", required = true)
                                            @RequestParam Long organizationId,
                                            @ApiParam(value = "排序对象", required = true)
                                            @RequestBody AdjustOrderVO adjustOrderVO) {
        return new ResponseEntity<>(objectSchemeFieldService.queryRank(adjustOrderVO.getPreviousRank(), adjustOrderVO.getNextRank()), HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "页面配置根据问题类型查询未选择的字段")
    @GetMapping(value = "/unselected")
    public ResponseEntity<List<ObjectSchemeFieldVO>> unselected(@ApiParam(value = "项目id", required = true)
                                                                @PathVariable("project_id") Long projectId,
                                                                @ApiParam(value = "组织id", required = true)
                                                                @RequestParam Long organizationId,
                                                                @ApiParam(value = "问题类型id", required = true)
                                                                @RequestParam @Encrypt Long issueTypeId) {
        return new ResponseEntity<>(objectSchemeFieldService.unselected(organizationId, projectId, issueTypeId), HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "根据方案编码获取人员自定义字段")
    @GetMapping("/member_list")
    public ResponseEntity<List<ObjectSchemeFieldVO>> selectMemberList(@ApiParam(value = "项目id", required = true)
                                                                       @PathVariable("project_id") Long projectId,
                                                                       @ApiParam(value = "组织id", required = true)
                                                                       @RequestParam Long organizationId,
                                                                       @ApiParam(value = "issue类型id", required = true)
                                                                       @RequestParam @Encrypt Long issueTypeId,
                                                                       @ApiParam(value = "方案编码")
                                                                       @RequestParam(required = false) String schemeCode) {
        return new ResponseEntity<>(objectSchemeFieldService.selectMemberList(organizationId, projectId, schemeCode, issueTypeId, null), HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "页面配置类型查询接口")
    @GetMapping(value = "/configs/issue_types")
    public ResponseEntity<List<IssueTypeVO>> issueTypes(@ApiParam(value = "项目id", required = true)
                                                        @PathVariable("project_id") Long projectId,
                                                        @ApiParam(value = "组织id", required = true)
                                                        @RequestParam Long organizationId) {
        return new ResponseEntity<>(objectSchemeFieldService.issueTypes(organizationId, projectId), HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "根据项目和类型查询描述模版")
    @GetMapping(value = "/description_template")
    public ResponseEntity<IssueTypeFieldVO> queryDescriptionTemplate(@ApiParam(value = "项目id", required = true)
                                                                     @PathVariable("project_id") Long projectId,
                                                                     @ApiParam(value = "组织id", required = true)
                                                                     @RequestParam Long organizationId,
                                                                     @ApiParam(value = "问题类型id", required = true)
                                                                     @RequestParam @Encrypt Long issueTypeId) {
        return new ResponseEntity<>(objectSchemeFieldService.queryDescriptionTemplate(projectId, issueTypeId, organizationId), HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "同步字段默认值到扩展字段类型")
    @PostMapping(value = "/sync_default_value")
    public ResponseEntity syncDefaultValue(@ApiParam(value = "项目id", required = true)
                                           @PathVariable("project_id") Long projectId,
                                           @ApiParam(value = "组织id", required = true)
                                           @RequestParam Long organizationId,
                                           @ApiParam(value = "字段id", required = true)
                                           @RequestParam("field_id") @Encrypt Long fieldId,
                                           @ApiParam(value = "更新字段", required = true)
                                           @RequestBody @Valid ObjectSchemeFieldUpdateVO updateDTO) {
        objectSchemeFieldService.syncDefaultValue(organizationId, projectId, fieldId, updateDTO);
        return new ResponseEntity<>(HttpStatus.CREATED);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "创建选项")
    @PostMapping("/{field_id}/options")
    public ResponseEntity<FieldOptionVO> insertOption(
            @ApiParam(value = "项目id", required = true)
            @PathVariable("project_id") Long projectId,
            @ApiParam(value = "字段id", required = true)
            @PathVariable("field_id") @Encrypt Long fieldId,
            @ApiParam(value = "组织id", required = true)
            @RequestParam Long organizationId,
            @ApiParam(value = "创建对象", required = true)
            @RequestBody @Valid FieldOptionUpdateVO fieldOptionUpdateVO) {
        return new ResponseEntity<>(fieldOptionService.insertOption(fieldOptionUpdateVO, fieldId, organizationId), HttpStatus.CREATED);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "更新选项")
    @PutMapping("/{field_id}/options/{option_id}")
    public ResponseEntity<FieldOptionVO> updateOption(
            @ApiParam(value = "项目id", required = true)
            @PathVariable("project_id") Long projectId,
            @ApiParam(value = "字段id", required = true)
            @PathVariable("field_id") @Encrypt Long fieldId,
            @ApiParam(value = "选项id", required = true)
            @PathVariable("option_id") @Encrypt Long optionId,
            @ApiParam(value = "组织id", required = true)
            @RequestParam Long organizationId,
            @ApiParam(value = "更新对象", required = true)
            @RequestBody @Valid FieldOptionUpdateVO fieldOptionUpdateVO) {
        fieldOptionUpdateVO.setId(optionId);
        return new ResponseEntity<>(fieldOptionService.updateOption(fieldOptionUpdateVO, fieldId, organizationId), HttpStatus.CREATED);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "删除选项")
    @DeleteMapping("/{field_id}/options/{option_id}")
    public ResponseEntity<Void> deleteOption(
            @ApiParam(value = "项目id", required = true)
            @PathVariable("project_id") Long projectId,
            @ApiParam(value = "字段id", required = true)
            @PathVariable("field_id") @Encrypt Long fieldId,
            @ApiParam(value = "选项id", required = true)
            @PathVariable("option_id") @Encrypt Long optionId,
            @ApiParam(value = "组织id", required = true)
            @RequestParam Long organizationId) {
        fieldOptionService.deleteOption(optionId, fieldId, organizationId);
        return new ResponseEntity<>(HttpStatus.OK);
    }
}
