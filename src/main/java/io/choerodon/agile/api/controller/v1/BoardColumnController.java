package io.choerodon.agile.api.controller.v1;

import io.choerodon.agile.api.vo.ColumnSortVO;
import io.choerodon.agile.api.vo.ColumnWithMaxMinNumVO;

import io.choerodon.core.iam.ResourceLevel;
import io.choerodon.swagger.annotation.Permission;
import io.choerodon.core.exception.CommonException;
import io.choerodon.agile.api.vo.BoardColumnVO;
import io.choerodon.agile.app.service.BoardColumnService;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import org.hzero.starter.keyencrypt.core.Encrypt;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Optional;

/**
 * Created by HuangFuqiang@choerodon.io on 2018/5/14.
 * Email: fuqianghuang01@gmail.com
 */
@RestController
@RequestMapping(value = "/v1/projects/{project_id}/board_column")
public class BoardColumnController {

    @Autowired
    private BoardColumnService boardColumnService;

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("创建BoardColumn")
    @PostMapping
    public ResponseEntity<BoardColumnVO> createBoardColumn(@ApiParam(value = "项目id", required = true)
                                                            @PathVariable(name = "project_id") Long projectId,
                                                           @ApiParam(value = "category code", required = true)
                                                            @RequestParam String categoryCode,
                                                           @ApiParam(value = "apply type", required = true)
                                                            @RequestParam String applyType,
                                                           @ApiParam(value = "board column对象", required = true)
                                                            @RequestBody BoardColumnVO boardColumnVO) {
        return Optional.ofNullable(boardColumnService.create(projectId, categoryCode, applyType, boardColumnVO))
                .map(result -> new ResponseEntity<>(result, HttpStatus.CREATED))
                .orElseThrow(() -> new CommonException("error.BoardColumn.create"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("更新BoardColumn")
    @PutMapping(value = "/{columnId}")
    public ResponseEntity<BoardColumnVO> updateBoardColumn(@ApiParam(value = "项目id", required = true)
                                                            @PathVariable(name = "project_id") Long projectId,
                                                           @ApiParam(value = "column id", required = true)
                                                            @PathVariable @Encrypt Long columnId,
                                                           @ApiParam(value = "board id", required = true)
                                                            @RequestParam @Encrypt Long boardId,
                                                           @ApiParam(value = "board column对象", required = true)
                                                            @RequestBody BoardColumnVO boardColumnVO) {
        return Optional.ofNullable(boardColumnService.update(projectId, columnId, boardId, boardColumnVO))
                .map(result -> new ResponseEntity<>(result, HttpStatus.CREATED))
                .orElseThrow(() -> new CommonException("error.BoardColumn.update"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("调整列的顺序")
    @PostMapping(value = "/column_sort")
    public ResponseEntity columnSort(@ApiParam(value = "项目id", required = true)
                                     @PathVariable(name = "project_id") Long projectId,
                                     @ApiParam(value = "ColumnSort DTO", required = true)
                                     @RequestBody ColumnSortVO columnSortVO) {
        boardColumnService.columnSort(0L, projectId, columnSortVO);
        return new ResponseEntity<>(HttpStatus.OK);
    }


    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("删除BoardColumn")
    @DeleteMapping(value = "/{columnId}")
    public ResponseEntity deleteBoardColumn(@ApiParam(value = "项目id", required = true)
                                            @PathVariable(name = "project_id") Long projectId,
                                            @ApiParam(value = "column id", required = true)
                                            @PathVariable @Encrypt Long columnId) {
        boardColumnService.delete(projectId, columnId);
        return new ResponseEntity<>(HttpStatus.NO_CONTENT);
    }


    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("根据id查询BoardColumn")
    @GetMapping(value = "/{columnId}")
    public ResponseEntity<BoardColumnVO> queryBoardColumnById(@ApiParam(value = "项目id", required = true)
                                                               @PathVariable(name = "project_id") Long projectId,
                                                              @ApiParam(value = "column id", required = true)
                                                               @PathVariable @Encrypt Long columnId) {
        return Optional.ofNullable(boardColumnService.queryBoardColumnById(projectId, columnId))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.BoardColumn.get"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("根据id更新最大最小值")
    @PostMapping(value = "/{columnId}/column_contraint")
    public ResponseEntity<BoardColumnVO> updateColumnContraint(@ApiParam(value = "项目id", required = true)
                                                                @PathVariable(name = "project_id") Long projectId,
                                                               @ApiParam(value = "column id", required = true)
                                                                @PathVariable @Encrypt Long columnId,
                                                               @ApiParam(value = "ColumnWithMaxMinNumVO", required = true)
                                                                @RequestBody ColumnWithMaxMinNumVO columnWithMaxMinNumVO) {
        return Optional.ofNullable(boardColumnService.updateColumnContraint(projectId, columnId, columnWithMaxMinNumVO))
                .map(result -> new ResponseEntity<>(result, HttpStatus.CREATED))
                .orElseThrow(() -> new CommonException("error.MaxAndMinNum.update"));
    }



}
