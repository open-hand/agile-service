package io.choerodon.agile.api.controller.v1;

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

import io.choerodon.agile.api.vo.CascadeFieldOptionSearchVO;
import io.choerodon.agile.api.vo.FieldCascadeRuleOptionVO;
import io.choerodon.agile.api.vo.FieldCascadeRuleVO;
import io.choerodon.agile.api.vo.PageConfigFieldVO;
import io.choerodon.agile.app.service.FieldCascadeRuleService;
import io.choerodon.core.exception.CommonException;
import io.choerodon.core.iam.ResourceLevel;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;
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
    @ApiOperation("查询级联规则可见选项")
    @GetMapping("/{field_cascade_rule_id}/option")
    public ResponseEntity<List<FieldCascadeRuleOptionVO>> listFieldCascadeRuleOptionByRule(
            @ApiParam(value = "项目id", required = true)
            @PathVariable(name = "project_id") Long projectId,
            @ApiParam(value = "级联规则id", required = true)
            @Encrypt @PathVariable(name = "field_cascade_rule_id") Long fieldCascadeRuleId) {
        return Optional.ofNullable(fieldCascadeRuleService.listFieldCascadeRuleOptionByRule(projectId, fieldCascadeRuleId))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.fieldCascadeRuleOption.list"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("级联规则批量更新、删除、创建")
    @PostMapping
    public ResponseEntity<List<FieldCascadeRuleVO>> batchMutationFieldCascadeRule(
            @ApiParam(value = "项目id", required = true)
            @PathVariable(name = "project_id") Long projectId,
            @ApiParam(value = "级联规则集合")
            @RequestBody List<FieldCascadeRuleVO> fieldCascadeRuleList) {
        return new ResponseEntity<>(fieldCascadeRuleService.batchMutationFieldCascadeRule(projectId, fieldCascadeRuleList), HttpStatus.CREATED);
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

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("查询级联字段下的可见选项")
    @PostMapping("/cascade_field/{cascade_field_id}/option")
    public ResponseEntity<Object> listCascadeFieldOption(
            @ApiParam(value = "项目id", required = true)
            @PathVariable(name = "project_id") Long projectId,
            @ApiParam(value = "级联字段id", required = true)
            @Encrypt @PathVariable(name = "cascade_field_id") Long cascadeFieldId,
            @ApiParam(value = "分页信息", required = true)
            @ApiIgnore PageRequest pageRequest,
            @ApiParam(value = "查询参数")
            @RequestBody CascadeFieldOptionSearchVO cascadeFieldOptionSearchVO) {
        return Optional.ofNullable(fieldCascadeRuleService.listCascadeFieldOption(
                projectId, cascadeFieldId, cascadeFieldOptionSearchVO, pageRequest))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.relPageFieldView.list"));
    }
}
