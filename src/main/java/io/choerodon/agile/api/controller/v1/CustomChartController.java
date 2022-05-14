package io.choerodon.agile.api.controller.v1;

import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.Optional;

import io.choerodon.agile.api.vo.CustomChartCreateVO;
import io.choerodon.agile.api.vo.CustomChartUpdateVO;
import io.choerodon.agile.api.vo.CustomChartVO;
import io.choerodon.agile.app.service.CustomChartService;
import io.choerodon.core.exception.CommonException;
import io.choerodon.core.iam.ResourceLevel;
import io.choerodon.swagger.annotation.Permission;

/**
 * @author chihao.ran@hand-china.com
 * 2021/06/21 16:31
 */
@RestController
@RequestMapping(value = "/v1/projects/{project_id}/custom_chart")
public class CustomChartController {

    @Autowired
    private CustomChartService customChartService;

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "查询项目下所有的自定义报表")
    @GetMapping
    public ResponseEntity<List<CustomChartVO>> queryListByProject(
            @ApiParam(value = "项目id", required = true)
            @PathVariable("project_id") Long projectId) {
        return Optional.ofNullable(customChartService.queryListByProject(projectId))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.custom.queryList"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "创建自定义报表")
    @PostMapping
    public ResponseEntity<CustomChartVO> createCustomChart(
            @ApiParam(value = "项目id", required = true)
            @PathVariable("project_id") Long projectId,
            @ApiParam(value = "自定义报表创建参数", required = true)
            @RequestBody @Validated CustomChartCreateVO customChartCreate) {
        return Optional.ofNullable(customChartService.createCustomChart(projectId, customChartCreate))
                .map(result -> new ResponseEntity<>(result, HttpStatus.CREATED))
                .orElseThrow(() -> new CommonException("error.custom.create"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "更新自定义报表")
    @PutMapping("/{custom_chart_id}")
    public ResponseEntity<CustomChartVO> updateCustomChart(
            @ApiParam(value = "项目id", required = true)
            @PathVariable("project_id") Long projectId,
            @ApiParam(value = "自定义报表id", required = true)
            @PathVariable("custom_chart_id") Long customChartId,
            @ApiParam(value = "更新参数", required = true)
            @RequestBody @Validated CustomChartUpdateVO customChartUpdate) {
        return Optional.ofNullable(customChartService.updateCustomChart(projectId, customChartId, customChartUpdate))
                .map(result -> new ResponseEntity<>(result, HttpStatus.CREATED))
                .orElseThrow(() -> new CommonException("error.custom.update"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "查询自定义报表详情页")
    @GetMapping("/{custom_chart_id}")
    public ResponseEntity<CustomChartVO> queryCustomChartDetail(
            @ApiParam(value = "项目id", required = true)
            @PathVariable("project_id") Long projectId,
            @ApiParam(value = "报表id", required = true)
            @PathVariable("custom_chart_id") Long customChartId) {
        return Optional.ofNullable(customChartService.queryCustomChartDetail(projectId, customChartId))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.custom.query"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "删除自定义报表")
    @DeleteMapping("/{custom_chart_id}")
    public ResponseEntity<Void> deleteCustomChart(
            @ApiParam(value = "项目id", required = true)
            @PathVariable("project_id") Long projectId,
            @ApiParam(value = "报表id", required = true)
            @PathVariable("custom_chart_id") Long customChartId) {
        customChartService.deleteCustomChartById(customChartId, projectId);
        return new ResponseEntity<>(HttpStatus.NO_CONTENT);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("自定义报表重名校验")
    @GetMapping(value = "/check_name")
    public ResponseEntity<Boolean> checkName(@ApiParam(value = "项目id", required = true)
                                             @PathVariable(name = "project_id") Long projectId,
                                             @ApiParam(value = "name", required = true)
                                             @RequestParam String name) {
        return Optional.ofNullable(customChartService.checkName(projectId, name))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.checkName.get"));
    }
}
