package io.choerodon.agile.api.controller.v1;

import org.hzero.core.util.Results;
import org.hzero.core.base.BaseController;
import io.choerodon.agile.infra.dto.ConfigurationRuleFiledDTO;
import io.choerodon.agile.app.service.ConfigurationRuleFiledService;
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
@RestController("configurationRuleFiledController.v1")
@RequestMapping("/v1/projects/{project_id}/configuration_rule_filed")
public class ConfigurationRuleFiledController extends BaseController {

    @Autowired
    private ConfigurationRuleFiledService configurationRuleFiledService;

    /**
     * 列表
     */
    @ApiOperation(value = "列表")
    @GetMapping
    public ResponseEntity<?> list(ConfigurationRuleFiledDTO configurationRuleFiledDTO, @ApiIgnore @SortDefault(value = ConfigurationRuleFiledDTO.FIELD_FIELD_CODE,
            direction = Sort.Direction.DESC) PageRequest pageRequest){
        Page<ConfigurationRuleFiledDTO> list = configurationRuleFiledService.pageAndSort(pageRequest, configurationRuleFiledDTO);
        return Results.success(list);
    }


    /**
     * 详细
     */
    @ApiOperation(value = "明细")
    @RequestMapping("/{fieldCode}")
    public ResponseEntity<?> detail(@PathVariable Long fieldCode) {
        ConfigurationRuleFiledDTO configurationRuleFiledDTO = configurationRuleFiledService.selectByPrimaryKey(fieldCode);
        return Results.success(configurationRuleFiledDTO);
    }

    /**
     * 创建
     */
    @ApiOperation(value = "创建")
    @PostMapping
    public ResponseEntity<?> create(@RequestBody ConfigurationRuleFiledDTO configurationRuleFiledDTO) {
            configurationRuleFiledService.insertSelective(configurationRuleFiledDTO);
        return Results.success(configurationRuleFiledDTO);
    }

    /**
     * 修改
     */
    @ApiOperation(value = "修改")
    @PutMapping
    public ResponseEntity<?> update(@RequestBody ConfigurationRuleFiledDTO configurationRuleFiledDTO) {
            configurationRuleFiledService.updateByPrimaryKeySelective(configurationRuleFiledDTO);
        return Results.success(configurationRuleFiledDTO);
    }

    /**
     * 删除
     */
    @ApiOperation(value = "删除")
    @DeleteMapping("/{fieldCode}")
    public ResponseEntity<?> remove(@PathVariable Long fieldCode) {
            configurationRuleFiledService.deleteByPrimaryKey(fieldCode);
        return Results.success();
    }

}
