package io.choerodon.agile.api.controller.v1;

import java.util.List;

import io.choerodon.agile.api.vo.ConfigurationRuleVO;
import io.choerodon.agile.app.service.ConfigurationRuleFiledService;
import io.choerodon.agile.app.service.ConfigurationRuleService;
import io.choerodon.agile.infra.dto.ConfigurationRuleDTO;
import io.choerodon.agile.infra.dto.ConfigurationRuleFiledDTO;
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

    public static final String FIELD_ID = "id";

    @Autowired
    private ConfigurationRuleService configurationRuleService;
    @Autowired
    private ConfigurationRuleFiledService configurationRuleFiledService;

    /**
     * 列表
     */
    @ApiOperation(value = "列表")
    @Permission(level = ResourceLevel.ORGANIZATION)
    @GetMapping
    public ResponseEntity<List<ConfigurationRuleVO>> list(@PathVariable("project_id") Long projectId,
                                                          @ApiIgnore @SortDefault(value = FIELD_ID,
            direction = Sort.Direction.DESC) PageRequest pageRequest){
        return Results.success(configurationRuleService.listByProjectId(projectId, pageRequest));
    }


    /**
     * 详细
     */
    @ApiOperation(value = "明细")
    @Permission(level = ResourceLevel.ORGANIZATION)
    @GetMapping("/{ruldId}")
    public ResponseEntity<ConfigurationRuleVO> detail(@PathVariable("project_id") Long projectId,
                                    @PathVariable Long ruldId) {
        return Results.success(configurationRuleService.queryById(projectId, ruldId));
    }

    /**
     * 创建
     */
    @ApiOperation(value = "创建")
    @Permission(level = ResourceLevel.ORGANIZATION)
    @PostMapping
    public ResponseEntity<Void> create(@PathVariable("project_id") Long projectId,
                                    @RequestBody ConfigurationRuleVO configurationRuleVO) {
        configurationRuleVO.setProjectId(projectId);
        configurationRuleService.create(projectId, configurationRuleVO);
        return Results.success();
    }

    /**
     * 修改
     */
    @ApiOperation(value = "修改")
    @Permission(level = ResourceLevel.ORGANIZATION)
    @PutMapping("/{ruldId}")
    public ResponseEntity<Void> update(@PathVariable("project_id") Long projectId,
                                    @PathVariable Long ruldId,
                                    @RequestBody ConfigurationRuleVO configurationRuleVO) {
        configurationRuleVO.setProjectId(projectId);
        configurationRuleVO.setId(ruldId);
        configurationRuleService.update(projectId, ruldId, configurationRuleVO);
        return Results.success();
    }

    /**
     * 删除
     */
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
    public ResponseEntity<List<ConfigurationRuleFiledDTO>> fieldList(@ApiParam(value = "项目id", required = true)
                                                         @PathVariable(name = "project_id") Long projectId) {
        return Results.success(configurationRuleFiledService.list(projectId));
    }

}
