package io.choerodon.agile.api.controller.v1;

import org.hzero.core.util.Results;
import org.hzero.core.base.BaseController;
import io.choerodon.agile.infra.dto.ConfigurationRuleReceiverDTO;
import io.choerodon.agile.app.service.ConfigurationRuleReceiverService;
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
@RestController("configurationRuleReceiverController.v1")
@RequestMapping("/v1/projects/{project_id}/configuration_rule_receiver")
public class ConfigurationRuleReceiverController extends BaseController {

    @Autowired
    private ConfigurationRuleReceiverService configurationRuleReceiverService;

    /**
     * 列表
     */
    @ApiOperation(value = "列表")
    @GetMapping
    public ResponseEntity<?> list(ConfigurationRuleReceiverDTO configurationRuleReceiverDTO, @ApiIgnore @SortDefault(value = ConfigurationRuleReceiverDTO.FIELD_ID,
            direction = Sort.Direction.DESC) PageRequest pageRequest){
        Page<ConfigurationRuleReceiverDTO> list = configurationRuleReceiverService.pageAndSort(pageRequest, configurationRuleReceiverDTO);
        return Results.success(list);
    }


    /**
     * 详细
     */
    @ApiOperation(value = "明细")
    @RequestMapping("/{id}")
    public ResponseEntity<?> detail(@PathVariable Long id) {
        ConfigurationRuleReceiverDTO configurationRuleReceiverDTO = configurationRuleReceiverService.selectByPrimaryKey(id);
        return Results.success(configurationRuleReceiverDTO);
    }

    /**
     * 创建
     */
    @ApiOperation(value = "创建")
    @PostMapping
    public ResponseEntity<?> create(@RequestBody ConfigurationRuleReceiverDTO configurationRuleReceiverDTO) {
            configurationRuleReceiverService.insertSelective(configurationRuleReceiverDTO);
        return Results.success(configurationRuleReceiverDTO);
    }

    /**
     * 修改
     */
    @ApiOperation(value = "修改")
    @PutMapping
    public ResponseEntity<?> update(@RequestBody ConfigurationRuleReceiverDTO configurationRuleReceiverDTO) {
            configurationRuleReceiverService.updateByPrimaryKeySelective(configurationRuleReceiverDTO);
        return Results.success(configurationRuleReceiverDTO);
    }

    /**
     * 删除
     */
    @ApiOperation(value = "删除")
    @DeleteMapping("/{id}")
    public ResponseEntity<?> remove(@PathVariable Long id) {
            configurationRuleReceiverService.deleteByPrimaryKey(id);
        return Results.success();
    }

}
