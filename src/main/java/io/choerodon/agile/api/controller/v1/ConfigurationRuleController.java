package io.choerodon.agile.api.controller.v1;

import java.util.List;

import io.choerodon.agile.api.vo.ConfigurationRuleVO;
import io.choerodon.agile.api.vo.ObjectSchemeFieldVO;
import io.choerodon.agile.app.service.ConfigurationRuleService;
import io.choerodon.agile.app.service.ObjectSchemeFieldService;
import io.choerodon.agile.infra.dto.ConfigurationRuleDTO;
import io.choerodon.core.domain.Page;
import io.choerodon.core.iam.ResourceLevel;
import io.choerodon.mybatis.pagehelper.annotation.SortDefault;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;
import io.choerodon.mybatis.pagehelper.domain.Sort;
import io.choerodon.swagger.annotation.Permission;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import org.hzero.core.base.BaseController;
import org.hzero.core.util.Results;
import org.hzero.starter.keyencrypt.core.Encrypt;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import springfox.documentation.annotations.ApiIgnore;

/**
 *  页面规则管理 API
 *
 * @author jiaxu.cui@hand-china.com
 * @date 2020-09-23 09:29:15
 */
@RestController
@RequestMapping("/v1/projects/{project_id}/configuration_rule")
public class ConfigurationRuleController extends BaseController {

    @Autowired
    private ConfigurationRuleService configurationRuleService;
    @Autowired
    private ObjectSchemeFieldService objectSchemeFieldService;

    @ApiOperation(value = "列表")
    @Permission(level = ResourceLevel.ORGANIZATION)
    @GetMapping
    public ResponseEntity<Page<ConfigurationRuleVO>> list(@PathVariable("project_id") Long projectId,
                                                          @ApiIgnore @SortDefault(value = ConfigurationRuleDTO.FIELD_ID,
            direction = Sort.Direction.DESC) PageRequest pageRequest){
        return Results.success(configurationRuleService.listByProjectId(projectId, pageRequest));
    }

    @ApiOperation(value = "明细")
    @Permission(level = ResourceLevel.ORGANIZATION)
    @GetMapping("/{ruleId}")
    public ResponseEntity<ConfigurationRuleVO> detail(@PathVariable("project_id") Long projectId,
                                               @PathVariable @Encrypt Long ruleId) {
        return Results.success(configurationRuleService.queryById(projectId, ruleId));
    }

    @ApiOperation(value = "创建")
    @Permission(level = ResourceLevel.ORGANIZATION)
    @PostMapping
    public ResponseEntity<Void> create(@PathVariable("project_id") Long projectId,
                                       @RequestBody ConfigurationRuleVO configurationRuleVO) {
        configurationRuleVO.setProjectId(projectId);
        configurationRuleService.create(projectId, configurationRuleVO);
        return Results.success();
    }
    
    @ApiOperation(value = "修改")
    @Permission(level = ResourceLevel.ORGANIZATION)
    @PutMapping("/{ruleId}")
    public ResponseEntity<Void> update(@PathVariable("project_id") Long projectId,
                                       @PathVariable @Encrypt Long ruleId,
                                       @RequestBody ConfigurationRuleVO configurationRuleVO) {
        configurationRuleVO.setProjectId(projectId);
        configurationRuleVO.setId(ruleId);
        configurationRuleService.update(projectId, ruleId, configurationRuleVO);
        return Results.success();
    }

    @ApiOperation(value = "删除")
    @Permission(level = ResourceLevel.ORGANIZATION)
    @DeleteMapping("/{ruleId}")
    public ResponseEntity<Void> remove(@PathVariable("project_id") Long projectId,
                                       @PathVariable @Encrypt Long ruleId) {
        configurationRuleService.deleteById(projectId, ruleId);
        return Results.success();
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("查询configuration rule field列表")
    @GetMapping("/fields")
    public ResponseEntity<List<ObjectSchemeFieldVO>> fieldList(@ApiParam(value = "项目id", required = true)
                                                               @PathVariable(name = "project_id") Long projectId,
                                                               @RequestParam("organizationId") Long organizationId,
                                                               @RequestParam("schemeCode") String schemeCode,
                                                               @RequestParam(value = "issueTypeList", required = false) List<String> issueTypeList) {
        return Results.success(objectSchemeFieldService.listPageFieldWithOption(organizationId, projectId, schemeCode, issueTypeList));
    }

    @ApiOperation(value = "启用")
    @Permission(level = ResourceLevel.ORGANIZATION)
    @PostMapping("/{ruleId}/enabled")
    public ResponseEntity<Void> enableRule(@PathVariable("project_id") Long projectId,
                                           @PathVariable @Encrypt Long ruleId) {
        configurationRuleService.changeRuleEnabled(projectId, ruleId, true);
        return Results.success();
    }

    @ApiOperation(value = "禁用")
    @Permission(level = ResourceLevel.ORGANIZATION)
    @PostMapping("/{ruleId}/disabled")
    public ResponseEntity<Void> disableRule(@PathVariable("project_id") Long projectId,
                                            @PathVariable @Encrypt Long ruleId) {
        configurationRuleService.changeRuleEnabled(projectId, ruleId, false);
        return Results.success();
    }

    @ApiOperation(value = "校验名称唯一")
    @Permission(level = ResourceLevel.ORGANIZATION)
    @GetMapping("/check_unique_name")
    public ResponseEntity<Boolean> checkUniqueName(@PathVariable("project_id") Long projectId,
                                            @RequestParam("name") String name) {
        return Results.success(configurationRuleService.checkUniqueName(projectId, name));
    }

}
