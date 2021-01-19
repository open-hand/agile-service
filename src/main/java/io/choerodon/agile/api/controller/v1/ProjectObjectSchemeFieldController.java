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
@RequestMapping("/v1/projects/{project_id}/object_scheme_field")
public class ProjectObjectSchemeFieldController {

    @Autowired
    private ObjectSchemeFieldService objectSchemeFieldService;

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
    public ResponseEntity<PageConfigVO> listConfigs(@PathVariable(name = "project_id") Long projectId,
                                                    @RequestParam Long organizationId,
                                                    @RequestParam String issueType) {
        return new ResponseEntity<>(objectSchemeFieldService.listConfigs(organizationId, projectId, issueType), HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "页面配置接口")
    @PostMapping(value = "/configs")
    public ResponseEntity<Boolean> config(@PathVariable(name = "project_id") Long projectId,
                                          @RequestParam Long organizationId,
                                          @RequestBody PageConfigUpdateVO pageConfigUpdateVO) {
        objectSchemeFieldService.config(organizationId, projectId, pageConfigUpdateVO);
        return new ResponseEntity<>(HttpStatus.OK);
    }


    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "更新字段是否必填")
    @PostMapping(value = "/update_required")
    public ResponseEntity updateRequired(@PathVariable("project_id") Long projectId,
                                         @RequestParam @Encrypt Long fieldId,
                                         @RequestParam Long organizationId,
                                         @RequestParam Boolean required) {
        objectSchemeFieldService.updateRequired(organizationId, projectId, fieldId, required);
        return new ResponseEntity<>(HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "拖动查询rank值接口")
    @PostMapping(value = "/rank")
    public ResponseEntity<String> queryRank(@PathVariable("project_id") Long projectId,
                                            @RequestParam Long organizationId,
                                            @RequestBody AdjustOrderVO adjustOrderVO) {
        return new ResponseEntity<>(objectSchemeFieldService.queryRank(adjustOrderVO.getPreviousRank(), adjustOrderVO.getNextRank()), HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "页面配置根据问题类型查询未选择的字段")
    @GetMapping(value = "/unselected")
    public ResponseEntity<List<ObjectSchemeFieldVO>> unselected(@PathVariable("project_id") Long projectId,
                                                                @RequestParam Long organizationId,
                                                                @RequestParam String issueType) {
        return new ResponseEntity<>(objectSchemeFieldService.unselected(organizationId, projectId, issueType), HttpStatus.OK);
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
    public ResponseEntity<List<IssueTypeVO>> issueTypes(@PathVariable("project_id") Long projectId,
                                                        @RequestParam Long organizationId) {
        return new ResponseEntity<>(objectSchemeFieldService.issueTypes(organizationId, projectId), HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "根据项目和类型查询描述模版")
    @GetMapping(value = "/description_template")
    public ResponseEntity<IssueTypeFieldVO> queryDescriptionTemplate(@PathVariable("project_id") Long projectId,
                                                                     @RequestParam Long organizationId,
                                                                     @RequestParam String issueType) {
        return new ResponseEntity<>(objectSchemeFieldService.queryDescriptionTemplate(projectId, issueType, organizationId), HttpStatus.OK);
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
                                           @ApiParam(value = "需要同步默认值的问题类型", required = true)
                                           @RequestParam("issue_types") String syncDefaultValueIssueTypes,
                                           @RequestBody @Valid  ObjectSchemeFieldUpdateVO updateDTO) {
        objectSchemeFieldService.syncDefaultValue(organizationId, projectId, fieldId, syncDefaultValueIssueTypes, updateDTO);
        return new ResponseEntity<>(HttpStatus.CREATED);
    }
}
