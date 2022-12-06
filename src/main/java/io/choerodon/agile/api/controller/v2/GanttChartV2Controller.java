package io.choerodon.agile.api.controller.v2;

import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.util.Assert;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;
import springfox.documentation.annotations.ApiIgnore;

import io.choerodon.agile.api.vo.GanttChartVO;
import io.choerodon.agile.api.vo.GanttDimensionListVO;
import io.choerodon.agile.api.vo.GanttDimensionMoveVO;
import io.choerodon.agile.api.vo.GanttMoveVO;
import io.choerodon.agile.api.vo.search.SearchParamVO;
import io.choerodon.agile.app.service.GanttChartService;
import io.choerodon.core.domain.Page;
import io.choerodon.core.iam.ResourceLevel;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;
import io.choerodon.swagger.annotation.Permission;

import org.hzero.core.base.BaseConstants;

/**
 * @author superlee
 * @since 2022-12-05
 */
@RestController
@RequestMapping(value = "/v2/projects/{project_id}/gantt")
public class GanttChartV2Controller {

    @Autowired
    private GanttChartService ganttChartService;

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("查询甘特图列表数据")
    @PostMapping(value = "/list")
    public ResponseEntity<Page<GanttChartVO>> pagedQuery(@ApiIgnore
                                                         @ApiParam(value = "分页信息", required = true)
                                                                 PageRequest pageRequest,
                                                         @ApiParam(value = "项目id", required = true)
                                                         @PathVariable(name = "project_id") Long projectId,
                                                         @ApiParam(value = "查询参数", required = true)
                                                         @RequestBody SearchParamVO searchParamVO) {
        return ResponseEntity.ok(ganttChartService.pagedQueryV2(projectId, searchParamVO, pageRequest));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("移动甘特图issue")
    @PostMapping(value = "/move")
    public ResponseEntity move(@ApiParam(value = "项目id", required = true)
                               @PathVariable(name = "project_id") Long projectId,
                               @ApiParam(value = "移动的甘特图对象", required = true)
                               @RequestBody @Validated GanttMoveVO ganttMoveVO) {
        Assert.notNull(ganttMoveVO.getSearchParamVO(), BaseConstants.ErrorCode.NOT_NULL);
        ganttChartService.moveV2(projectId, ganttMoveVO);
        return new ResponseEntity(HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("查询甘特图纬度数据")
    @PostMapping(value = "/list_dimension")
    public ResponseEntity<GanttDimensionListVO> ganttDimensionList(@ApiParam(value = "项目id", required = true)
                                                                   @PathVariable(name = "project_id") Long projectId,
                                                                   @ApiParam(value = "查询参数", required = true)
                                                                   @RequestBody SearchParamVO searchParamVO) {
        return ResponseEntity.ok(ganttChartService.ganttDimensionListV2(projectId, searchParamVO));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("移动甘特图冲刺/经办人")
    @PostMapping(value = "/move_dimension")
    public ResponseEntity moveDimension(@ApiParam(value = "项目id", required = true)
                                        @PathVariable(name = "project_id") Long projectId,
                                        @ApiParam(value = "移动的冲刺/经办人对象", required = true)
                                        @RequestBody GanttDimensionMoveVO ganttDimensionMoveVO) {
        Assert.notNull(ganttDimensionMoveVO.getSearchParamVO(), BaseConstants.ErrorCode.NOT_NULL);
        ganttChartService.moveDimensionV2(projectId, ganttDimensionMoveVO);
        return new ResponseEntity(HttpStatus.OK);
    }
}
