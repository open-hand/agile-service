package io.choerodon.agile.api.controller.v1;

import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import org.hzero.starter.keyencrypt.core.Encrypt;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.Optional;

import io.choerodon.agile.api.vo.FieldCascadeCreateVO;
import io.choerodon.agile.api.vo.FieldCascadeRuleVO;
import io.choerodon.agile.api.vo.FieldCascadeUpdateVO;
import io.choerodon.agile.api.vo.PageConfigFieldVO;
import io.choerodon.agile.app.service.FieldCascadeRuleService;
import io.choerodon.core.exception.CommonException;
import io.choerodon.core.iam.ResourceLevel;
import io.choerodon.swagger.annotation.Permission;

/**
 * @author chihao.ran@hand-china.com
 * 2021/07/13 15:58
 */
@RestController
@RequestMapping(value = "/v1/projects/{project_id}/field_cascade_rule")
public class FieldCascadeRuleController {

    @Autowired
    private FieldCascadeRuleService fieldCascadeRuleService;

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("根据问题类型查询级联规则")
    @GetMapping
    public ResponseEntity<List<FieldCascadeRuleVO>> listFieldCascadeRuleByIssueType(
            @ApiParam(value = "项目id", required = true)
            @PathVariable(name = "project_id") Long projectId,
            @ApiParam(value = "问题类型id", required = true)
            @Encrypt @RequestParam(name = "issue_type_id") Long issueTypeId,
            @ApiParam(value = "字段id")
            @Encrypt @RequestParam(name = "field_id", required = false) Long fieldId) {
        return Optional.ofNullable(fieldCascadeRuleService.listFieldCascadeRuleByIssueType(projectId, issueTypeId, fieldId))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.fieldCascadeRule.list"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("查询级联规则详情")
    @GetMapping("/{field_cascade_rule_id}")
    public ResponseEntity<FieldCascadeRuleVO> fieldCascadeRuleDetail(
            @ApiParam(value = "项目id", required = true)
            @PathVariable(name = "project_id") Long projectId,
            @ApiParam(value = "级联规则id", required = true)
            @Encrypt @PathVariable(name = "field_cascade_rule_id") Long fieldCascadeRuleId) {
        return Optional.ofNullable(fieldCascadeRuleService.fieldCascadeRuleDetail(projectId, fieldCascadeRuleId))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.fieldCascadeRule.detail"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("创建级联规则")
    @PostMapping
    public ResponseEntity<FieldCascadeRuleVO> createFieldCascadeRule(
            @ApiParam(value = "项目id", required = true)
            @PathVariable(name = "project_id") Long projectId,
            @Validated @RequestBody FieldCascadeCreateVO fieldCascadeCreate) {
        return new ResponseEntity<>(fieldCascadeRuleService.createFieldCascadeRule(projectId, fieldCascadeCreate), HttpStatus.CREATED);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("更新级联规则")
    @PutMapping("/{field_cascade_rule_id}")
    public ResponseEntity<FieldCascadeRuleVO> updateFieldCascadeRule(
            @ApiParam(value = "项目id", required = true)
            @PathVariable(name = "project_id") Long projectId,
            @ApiParam(value = "级联规则id", required = true)
            @Encrypt @PathVariable(name = "field_cascade_rule_id") Long fieldCascadeRuleId,
            @RequestBody FieldCascadeUpdateVO fieldCascadeUpdate) {
        return new ResponseEntity<>(fieldCascadeRuleService.updateFieldCascadeRule(projectId, fieldCascadeRuleId, fieldCascadeUpdate), HttpStatus.CREATED);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("删除级联规则")
    @DeleteMapping("/{field_cascade_rule_id}")
    public ResponseEntity<Void> deleteFieldCascadeRule(
            @ApiParam(value = "项目id", required = true)
            @PathVariable(name = "project_id") Long projectId,
            @ApiParam(value = "级联规则id", required = true)
            @Encrypt @PathVariable(name = "field_cascade_rule_id") Long fieldCascadeRuleId) {
        fieldCascadeRuleService.deleteFieldCascadeRule(projectId, fieldCascadeRuleId);
        return new ResponseEntity<>(HttpStatus.NO_CONTENT);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("根据问题类型查询可级联字段")
    @GetMapping("/no_loop_filed")
    public ResponseEntity<List<PageConfigFieldVO>> listCascadePageFieldView(
            @ApiParam(value = "项目id", required = true)
            @PathVariable(name = "project_id") Long projectId,
            @ApiParam(value = "问题类型id", required = true)
            @Encrypt @RequestParam(name = "issue_type_id") Long issueTypeId,
            @ApiParam(value = "字段id", required = true)
            @Encrypt @RequestParam(name = "field_id") Long fieldId) {
        return Optional.ofNullable(fieldCascadeRuleService.listCascadePageFieldView(projectId, issueTypeId, fieldId))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.relPageFieldView.list"));
    }
}
