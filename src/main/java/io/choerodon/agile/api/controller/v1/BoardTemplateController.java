package io.choerodon.agile.api.controller.v1;

import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.app.service.BoardColumnService;
import io.choerodon.agile.app.service.BoardService;
import io.choerodon.agile.app.service.BoardTemplateService;
import io.choerodon.core.exception.CommonException;
import io.choerodon.core.iam.ResourceLevel;
import io.choerodon.swagger.annotation.Permission;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import org.hzero.starter.keyencrypt.core.Encrypt;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.Optional;

/**
 * @author zhaotianxin
 * @date 2021-03-24 10:52
 */
@RestController
@RequestMapping(value = "/v1/organizations/{organization_id}/board_template")
public class BoardTemplateController {
    @Autowired
    private BoardService boardService;
    @Autowired
    private BoardColumnService boardColumnService;
    @Autowired
    private BoardTemplateService boardTemplateService;

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("创建scrum board,创建默认列，关联项目状态")
    @PostMapping("/board/create")
    public ResponseEntity<Void> createBoardTemplate(@ApiParam(value = "组织Id", required = true)
                                                    @PathVariable(name = "organization_id") Long organizationId,
                                                    @ApiParam(value = "board name", required = true)
                                                    @RequestParam String boardName) {
        boardTemplateService.createBoardTemplate(organizationId, boardName);
        return new ResponseEntity<>(HttpStatus.CREATED);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("更新scrum board")
    @PutMapping(value = "/board/{boardId}")
    public ResponseEntity<BoardVO> updateBoardTemplate(@ApiParam(value = "组织Id", required = true)
                                                       @PathVariable(name = "organization_id") Long organizationId,
                                                       @ApiParam(value = "agile board id", required = true)
                                                       @PathVariable @Encrypt Long boardId,
                                                       @ApiParam(value = "ScrumBoard对象", required = true)
                                                       @RequestBody BoardVO boardVO) {
        return Optional.ofNullable(boardTemplateService.updateBoardTemplate(organizationId, boardId, boardVO))
                .map(result -> new ResponseEntity<>(result, HttpStatus.CREATED))
                .orElseThrow(() -> new CommonException("error.board.template.update"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("删除scrum board")
    @DeleteMapping("/board/{boardId}")
    public ResponseEntity<Void> deleteBoardTemplate(@ApiParam(value = "组织Id", required = true)
                                                    @PathVariable(name = "organization_id") Long organizationId,
                                                    @ApiParam(value = "agile board id", required = true)
                                                    @PathVariable @Encrypt Long boardId) {
        boardTemplateService.deleteBoardTemplate(organizationId, boardId);
        return new ResponseEntity<>(HttpStatus.NO_CONTENT);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("查询组织下创建的看板模板")
    @GetMapping(value = "/board/list")
    public ResponseEntity<List<BoardVO>> listBoardTemplate(@ApiParam(value = "组织Id", required = true)
                                                           @PathVariable(name = "organization_id") Long organizationId) {
        return Optional.ofNullable(boardTemplateService.listBoardTemplate(organizationId))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.board.template.query"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("根据id查询scrum board")
    @GetMapping(value = "/board/{boardId}")
    public ResponseEntity<BoardVO> queryBoardTemplateById(@ApiParam(value = "组织Id", required = true)
                                                          @PathVariable(name = "organization_id") Long organizationId,
                                                          @ApiParam(value = "agile board id", required = true)
                                                          @PathVariable @Encrypt Long boardId) {
        return Optional.ofNullable(boardTemplateService.queryBoardTemplateById(organizationId, boardId))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.board.template.get"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("校验看板名称重复性")
    @GetMapping("/board/check_name")
    public ResponseEntity<Boolean> checkName(@ApiParam(value = "组织Id", required = true)
                                             @PathVariable(name = "organization_id") Long organizationId,
                                             @ApiParam(value = "board name", required = true)
                                             @RequestParam String boardName) {
        return Optional.ofNullable(boardService.checkName(organizationId, 0L, boardName))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.check.template.name.get"));

    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("创建BoardColumn")
    @PostMapping("/board_column/create")
    public ResponseEntity<BoardColumnVO> createBoardColumn(@ApiParam(value = "组织Id", required = true)
                                                           @PathVariable(name = "organization_id") Long organizationId,
                                                           @ApiParam(value = "category code", required = true)
                                                           @RequestParam String categoryCode,
                                                           @ApiParam(value = "board column对象", required = true)
                                                           @RequestBody BoardColumnVO boardColumnVO) {
        return Optional.ofNullable(boardTemplateService.createBoardColumn(organizationId, categoryCode, boardColumnVO))
                .map(result -> new ResponseEntity<>(result, HttpStatus.CREATED))
                .orElseThrow(() -> new CommonException("error.board.template.column.create"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("更新BoardColumn")
    @PutMapping(value = "/board_column/{columnId}")
    public ResponseEntity<BoardColumnVO> updateBoardColumn(@ApiParam(value = "组织Id", required = true)
                                                           @PathVariable(name = "organization_id") Long organizationId,
                                                           @ApiParam(value = "column id", required = true)
                                                           @PathVariable @Encrypt Long columnId,
                                                           @ApiParam(value = "board id", required = true)
                                                           @RequestParam @Encrypt Long boardId,
                                                           @ApiParam(value = "board column对象", required = true)
                                                           @RequestBody BoardColumnVO boardColumnVO) {
        return Optional.ofNullable(boardTemplateService.updateBoardColumn(organizationId, columnId, boardId, boardColumnVO))
                .map(result -> new ResponseEntity<>(result, HttpStatus.CREATED))
                .orElseThrow(() -> new CommonException("error.board.template.column.update"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("调整列的顺序")
    @PostMapping(value = "/board_column/column_sort")
    public ResponseEntity columnSort(@ApiParam(value = "组织Id", required = true)
                                     @PathVariable(name = "organization_id") Long organizationId,
                                     @ApiParam(value = "ColumnSort DTO", required = true)
                                     @RequestBody ColumnSortVO columnSortVO) {
        boardColumnService.columnSort(organizationId,0L, columnSortVO);
        return new ResponseEntity<>(HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("根据id更新最大最小值")
    @PostMapping(value = "/board_column/{columnId}/column_contraint")
    public ResponseEntity<BoardColumnVO> updateColumnContraint(@ApiParam(value = "组织Id", required = true)
                                                               @PathVariable(name = "organization_id") Long organizationId,
                                                               @ApiParam(value = "column id", required = true)
                                                               @PathVariable @Encrypt Long columnId,
                                                               @ApiParam(value = "ColumnWithMaxMinNumVO", required = true)
                                                               @RequestBody ColumnWithMaxMinNumVO columnWithMaxMinNumVO) {
        return Optional.ofNullable(boardTemplateService.updateColumnContraintTemplate(organizationId, columnId, columnWithMaxMinNumVO))
                .map(result -> new ResponseEntity<>(result, HttpStatus.CREATED))
                .orElseThrow(() -> new CommonException("error.template.MaxAndMinNum.update"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("状态移动至列中")
    @PostMapping(value = "/board_column/{statusId}/move_to_column")
    public ResponseEntity<StatusVO> moveStatusToColumn(@ApiParam(value = "组织Id", required = true)
                                                       @PathVariable(name = "organization_id") Long organizationId,
                                                       @ApiParam(value = "状态statusId", required = true)
                                                       @PathVariable @Encrypt Long statusId,
                                                       @ApiParam(value = "status move object", required = true)
                                                       @RequestBody StatusMoveVO statusMoveVO) {
        return Optional.ofNullable(boardTemplateService.moveStatusToColumn(organizationId, statusId, statusMoveVO))
                .map(result -> new ResponseEntity<>(result, HttpStatus.CREATED))
                .orElseThrow(() -> new CommonException("error.board.template.status.move"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("状态移动至未对应")
    @PostMapping(value = "/board_column/{statusId}/move_to_uncorrespond")
    public ResponseEntity<StatusVO> moveStatusToUnCorrespond(@ApiParam(value = "组织Id", required = true)
                                                             @PathVariable(name = "organization_id") Long organizationId,
                                                             @ApiParam(value = "状态id", required = true)
                                                             @PathVariable @Encrypt Long statusId,
                                                             @ApiParam(value = "status move object", required = true)
                                                             @RequestBody StatusMoveVO statusMoveVO) {
        return Optional.ofNullable(boardTemplateService.moveStatusToUnCorrespond(organizationId, statusId, statusMoveVO))
                .map(result -> new ResponseEntity<>(result, HttpStatus.CREATED))
                .orElseThrow(() -> new CommonException("error.board.template.status.move"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("查询看板所有的列和状态")
    @GetMapping(value = "/board_column/{board_id}/list")
    public ResponseEntity<List<BoardColumnVO>> listColumnByBoardId(@ApiParam(value = "组织Id", required = true)
                                                                   @PathVariable(name = "organization_id") Long organizationId,
                                                                   @ApiParam(value = "看板Id", required = true)
                                                                   @PathVariable(name = "board_id") @Encrypt Long boardId) {
        return Optional.ofNullable(boardTemplateService.listColumnByBoardId(organizationId, boardId))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.board.template.column.query"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("查询看板所有的列和状态")
    @PutMapping(value = "/board_column/{status_id}/status_template_completed")
    public ResponseEntity<StatusTemplateVO> settingStatusTemplate(@ApiParam(value = "组织Id", required = true)
                                                                @PathVariable(name = "organization_id") Long organizationId,
                                                                @ApiParam(value = "状态Id", required = true)
                                                                @PathVariable(name = "status_id") @Encrypt Long statusId,
                                                                @ApiParam(value = "设置模板状态是否是已完成状态", required = true)
                                                                @RequestParam Boolean completed) {
        return Optional.ofNullable(boardTemplateService.settingStatusTemplate(organizationId, statusId, completed))
                .map(result -> new ResponseEntity<>(result, HttpStatus.CREATED))
                .orElseThrow(() -> new CommonException("error.status.template.update"));
    }

}
