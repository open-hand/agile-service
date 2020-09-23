package io.choerodon.agile.api.controller.v1;

import io.choerodon.agile.api.vo.ConfigurationRuleVO;
import io.choerodon.core.iam.ResourceLevel;
import io.choerodon.swagger.annotation.Permission;
import org.hzero.core.util.Results;
import org.hzero.core.base.BaseController;
import io.choerodon.agile.infra.dto.ConfigurationRuleDTO;
import io.choerodon.agile.app.service.ConfigurationRuleService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import io.choerodon.core.domain.Page;
import io.choerodon.mybatis.pagehelper.annotation.SortDefault;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;
import io.choerodon.mybatis.pagehelper.domain.Sort;
import io.swagger.annotations.ApiOperation;
import springfox.documentation.annotations.ApiIgnore;

/**
 *  页面规则管理 API
 *
 * @author jiaxu.cui@hand-china.com
 * @date 2020-09-23 09:29:15
 */
@RestController("configurationRuleController.v1")
@RequestMapping("/v1/projects/{project_id}/configuration_rule")
public class ConfigurationRuleController extends BaseController {

    @Autowired
    private ConfigurationRuleService configurationRuleService;

    /**
     * 列表
     */
    @ApiOperation(value = "列表")
    @Permission(level = ResourceLevel.ORGANIZATION)
    @GetMapping
    public ResponseEntity<?> list(ConfigurationRuleDTO configurationRuleDTO, @ApiIgnore @SortDefault(value = ConfigurationRuleDTO.FIELD_ID,
            direction = Sort.Direction.DESC) PageRequest pageRequest){
        Page<ConfigurationRuleDTO> list = configurationRuleService.pageAndSort(pageRequest, configurationRuleDTO);
        return Results.success(list);
    }


    /**
     * 详细
     */
    @ApiOperation(value = "明细")
    @Permission(level = ResourceLevel.ORGANIZATION)
    @RequestMapping("/{id}")
    public ResponseEntity<?> detail(@PathVariable Long id) {
        ConfigurationRuleDTO configurationRuleDTO = configurationRuleService.selectByPrimaryKey(id);
        return Results.success(configurationRuleDTO);
    }

    /**
     * 创建
     */
    @ApiOperation(value = "创建")
    @Permission(level = ResourceLevel.ORGANIZATION)
    @PostMapping
    public ResponseEntity<?> create(@PathVariable("project_id") Long projectId,
                                    @RequestBody ConfigurationRuleVO configurationRuleVO) {
        configurationRuleVO.setProjectId(projectId);
        return Results.success(configurationRuleService.create(projectId, configurationRuleVO));
    }

    /**
     * 修改
     */
    @ApiOperation(value = "修改")
    @Permission(level = ResourceLevel.ORGANIZATION)
    @PutMapping("/{id}")
    public ResponseEntity<?> update(@PathVariable("project_id") Long projectId,
                                    @PathVariable("id") Long id,
                                    @RequestBody ConfigurationRuleVO configurationRuleVO) {
        configurationRuleVO.setProjectId(projectId);
        configurationRuleVO.setId(id);
        return Results.success(configurationRuleService.update(projectId, id, configurationRuleVO));
    }

    /**
     * 删除
     */
    @ApiOperation(value = "删除")
    @Permission(level = ResourceLevel.ORGANIZATION)
    @DeleteMapping("/{id}")
    public ResponseEntity<?> remove(@PathVariable Long id) {
            configurationRuleService.deleteByPrimaryKey(id);
        return Results.success();
    }

}
