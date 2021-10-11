package io.choerodon.agile.api.controller.v1;

import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.app.service.GanttChartService;
import io.choerodon.agile.infra.utils.EncryptionUtils;
import io.choerodon.core.domain.Page;
import io.choerodon.core.iam.ResourceLevel;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;
import io.choerodon.swagger.annotation.Permission;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import org.hzero.starter.keyencrypt.core.Encrypt;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;
import springfox.documentation.annotations.ApiIgnore;

import java.util.List;
import java.util.Set;


/**
 * @author superlee
 * @since 2020-11-24
 */
@RestController
@RequestMapping(value = "/v1/projects/{project_id}/gantt")
public class GanttChartController {

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
                                                         @RequestBody(required = false) SearchVO searchVO) {
        EncryptionUtils.decryptSearchVO(searchVO);
        return ResponseEntity.ok(ganttChartService.pagedQuery(projectId, searchVO, pageRequest));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("根据issueId查询甘特图列表数据")
    @PostMapping(value = "/list_by_ids")
    public ResponseEntity<List<GanttChartVO>> listByIds(@ApiParam(value = "项目id", required = true)
                                                        @PathVariable(name = "project_id") Long projectId,
                                                        @RequestBody @Encrypt Set<Long> issueIds,
                                                        @RequestParam String dimension) {
        return ResponseEntity.ok(ganttChartService.listByIds(projectId, issueIds, dimension));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("移动甘特图issue")
    @PostMapping(value = "/move")
    public ResponseEntity move(@ApiParam(value = "项目id", required = true)
                               @PathVariable(name = "project_id") Long projectId,
                               @RequestBody @Validated GanttMoveVO ganttMoveVO) {
        EncryptionUtils.decryptSearchVO(ganttMoveVO.getSearchVO());
        ganttChartService.move(projectId, ganttMoveVO);
        return new ResponseEntity(HttpStatus.OK);
    }


    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("查询甘特图列表数据")
    @PostMapping(value = "/list_dimension")
    public ResponseEntity<GanttDimensionListVO> ganttDimensionList(@ApiParam(value = "项目id", required = true)
                                                                   @PathVariable(name = "project_id") Long projectId,
                                                                   @ApiParam(value = "查询参数", required = true)
                                                                   @RequestBody(required = false) SearchVO searchVO) {
        EncryptionUtils.decryptSearchVO(searchVO);
        return ResponseEntity.ok(ganttChartService.ganttDimensionList(projectId, searchVO));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("移动甘特图冲刺/经办人")
    @PostMapping(value = "/move_dimension")
    public ResponseEntity moveDimension(@ApiParam(value = "项目id", required = true)
                                        @PathVariable(name = "project_id") Long projectId,
                                        @RequestBody @Validated GanttDimensionMoveVO ganttDimensionMoveVO) {
        EncryptionUtils.decryptSearchVO(ganttDimensionMoveVO.getSearchVO());
        ganttChartService.moveDimension(projectId, ganttDimensionMoveVO);
        return new ResponseEntity(HttpStatus.OK);
    }
}
