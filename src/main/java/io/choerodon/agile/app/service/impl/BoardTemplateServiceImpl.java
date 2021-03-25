package io.choerodon.agile.app.service.impl;

import io.choerodon.agile.api.validator.BoardValidator;
import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.app.service.*;
import io.choerodon.agile.infra.dto.*;
import io.choerodon.agile.infra.mapper.BoardColumnMapper;
import io.choerodon.agile.infra.mapper.BoardMapper;
import io.choerodon.agile.infra.mapper.StatusTemplateMapper;
import io.choerodon.core.exception.CommonException;
import org.apache.commons.collections4.CollectionUtils;
import org.modelmapper.ModelMapper;
import org.modelmapper.TypeToken;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.*;

import static java.util.Comparator.naturalOrder;
import static java.util.Comparator.nullsFirst;

/**
 * @author zhaotianxin
 * @date 2021-03-25 14:37
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class BoardTemplateServiceImpl implements BoardTemplateService {

    private static final String TODO = "待处理";
    private static final String DOING = "处理中";
    private static final String DONE = "已完成";
    private static final String TODO_CODE = "todo";
    private static final String DOING_CODE = "doing";
    private static final String DONE_CODE = "done";
    private static final Integer SEQUENCE_ONE = 0;
    private static final Integer SEQUENCE_TWO = 1;
    private static final Integer SEQUENCE_THREE = 2;
    private static final String COLUMN_COLOR_TODO = "column_color_todo";
    private static final String COLUMN_COLOR_DOING = "column_color_doing";
    private static final String COLUMN_COLOR_DONE = "column_color_done";

    @Autowired
    private BoardMapper boardMapper;

    @Autowired
    private BoardService boardService;

    @Autowired
    private ModelMapper modelMapper;

    @Autowired
    private BoardColumnMapper boardColumnMapper;

    @Autowired
    private ColumnStatusRelService columnStatusRelService;

    @Autowired
    private StatusService statusService;

    @Autowired
    private BoardColumnService boardColumnService;

    @Autowired
    private StatusTemplateMapper statusTemplateMapper;

    @Autowired
    private IssueStatusService issueStatusService;

    @Override
    public void createBoardTemplate(Long organizationId, String boardName) {
        if (Boolean.TRUE.equals(boardService.checkName(organizationId, 0L, boardName))) {
            throw new CommonException("error.board.template.name.exist");
        }
        BoardDTO boardResult = boardService.createBoard(organizationId, 0L, boardName);
        createTemplateColumnWithRelateStatus(boardResult);
    }

    private void createTemplateColumnWithRelateStatus(BoardDTO boardResult) {
        List<ColumnWithStatusRelDTO> columnWithStatusRelDTOList = new ArrayList<>();
        Long boardId = boardResult.getBoardId();
        Long organizationId = boardResult.getOrganizationId();
        List<StatusVO> statusMapDTOMap = statusService.queryAllStatus(organizationId);
        String[] codes = {"create", "processing", "complete"};
        List<String> codeList = Arrays.asList(codes);
        for (StatusVO statusVO : statusMapDTOMap) {
            if (codeList.contains(statusVO.getCode())) {
                ColumnWithStatusRelDTO columnWithStatusRelDTO = new ColumnWithStatusRelDTO();
                columnWithStatusRelDTO.setStatusId(statusVO.getId());
                columnWithStatusRelDTO.setCategoryCode(statusVO.getType());
                columnWithStatusRelDTOList.add(columnWithStatusRelDTO);
            }
        }
        boardColumnService.relate(organizationId,0L, boardId, TODO, TODO_CODE, SEQUENCE_ONE, columnWithStatusRelDTOList, COLUMN_COLOR_TODO);
        boardColumnService.relate(organizationId,0L, boardId, DOING, DOING_CODE, SEQUENCE_TWO, columnWithStatusRelDTOList, COLUMN_COLOR_DOING);
        boardColumnService.relate(organizationId,0L, boardId, DONE, DONE_CODE, SEQUENCE_THREE, columnWithStatusRelDTOList, COLUMN_COLOR_DONE);
    }

    @Override
    public BoardVO updateBoardTemplate(Long organizationId, Long boardId, BoardVO boardVO) {
        BoardDTO boardDTO = boardMapper.selectByPrimaryKey(boardId);
        if (Objects.equals(boardDTO.getName(), boardVO.getName())) {
            throw new CommonException("error.boardName.exist");
        }
        if (boardVO.getName() != null && boardService.checkName(organizationId, 0L, boardVO.getName())) {
            throw new CommonException("error.boardName.exist");
        }
        boardVO.setBoardId(boardId);
        if (boardMapper.updateByPrimaryKeySelective(modelMapper.map(boardVO, BoardDTO.class)) != 1) {
            throw new CommonException("error.board.update");
        }
        return modelMapper.map(boardMapper.selectByPrimaryKey(boardVO.getBoardId()), BoardVO.class);
    }

    @Override
    public void deleteBoardTemplate(Long organizationId, Long boardId) {
        BoardDTO boardDTO = new BoardDTO();
        boardDTO.setOrganizationId(organizationId);
        boardDTO.setProjectId(0L);
        List<BoardDTO> boardDTOS = boardMapper.select(boardDTO);
        BoardValidator.checkBoardUnique(boardDTOS);

        BoardColumnDTO boardColumnDTO = new BoardColumnDTO();
        boardColumnDTO.setBoardId(boardId);
        List<BoardColumnDTO> boardColumnDTOList = boardColumnMapper.select(boardColumnDTO);
        for (BoardColumnDTO column : boardColumnDTOList) {
            ColumnStatusRelDTO columnStatusRelDTO = new ColumnStatusRelDTO();
            columnStatusRelDTO.setColumnId(column.getColumnId());
            columnStatusRelDTO.setProjectId(0L);
            columnStatusRelDTO.setOrganizationId(organizationId);
            columnStatusRelService.delete(columnStatusRelDTO);
            if (boardColumnMapper.deleteByPrimaryKey(column.getColumnId()) != 1) {
                throw new CommonException("error.BoardColumn.delete");
            }
        }
        if (boardMapper.deleteByPrimaryKey(boardId) != 1) {
            throw new CommonException("error.board.delete");
        }
    }

    @Override
    public BoardVO queryBoardTemplateById(Long organizationId, Long boardId) {
        BoardDTO boardDTO = new BoardDTO();
        boardDTO.setProjectId(0L);
        boardDTO.setOrganizationId(organizationId);
        boardDTO.setBoardId(boardId);
        return modelMapper.map(boardMapper.selectOne(boardDTO), BoardVO.class);
    }

    @Override
    public List<BoardVO> listBoardTemplate(Long organizationId) {
        BoardDTO boardDTO = new BoardDTO();
        boardDTO.setProjectId(0L);
        boardDTO.setOrganizationId(organizationId);
        List<BoardDTO> boardDTOS = boardMapper.select(boardDTO);
        if (CollectionUtils.isEmpty(boardDTOS)) {
            return new ArrayList<>();
        }
        return modelMapper.map(boardDTOS, new TypeToken<List<BoardVO>>(){}.getType());
    }

    @Override
    public BoardColumnVO createBoardColumn(Long organizationId, String categoryCode, BoardColumnVO boardColumnVO) {
        // 创建列
        boardColumnService.createCheck(boardColumnVO);
        StatusVO statusVO = new StatusVO();
        statusVO.setType(categoryCode);
        statusVO.setName(boardColumnVO.getName());
        statusVO = statusService.create(organizationId, statusVO);
        if (statusVO != null && statusVO.getId() != null) {
            Long statusId = statusVO.getId();
            Boolean checkStatus = boardColumnService.checkColumnStatusExist(organizationId,0L, statusId);
            boardColumnService.setColumnColor(boardColumnVO, checkStatus);
            BoardColumnDTO columnDTO = modelMapper.map(boardColumnVO, BoardColumnDTO.class);
            columnDTO.setProjectId(0L);
            columnDTO.setOrganizationId(organizationId);
            BoardColumnDTO boardColumnDTO = boardColumnService.createBase(columnDTO);
            // 创建列与状态关联关系
            if (Boolean.FALSE.equals(checkStatus)) {
                ColumnStatusRelDTO columnStatusRelDTO = new ColumnStatusRelDTO();
                columnStatusRelDTO.setColumnId(boardColumnDTO.getColumnId());
                columnStatusRelDTO.setStatusId(statusId);
                columnStatusRelDTO.setPosition(0);
                columnStatusRelDTO.setProjectId(0L);
                columnStatusRelDTO.setOrganizationId(organizationId);
                columnStatusRelService.create(columnStatusRelDTO);
            }
            return boardColumnVO;
        } else {
            boardColumnService.setColumnColor(boardColumnVO, true);
            BoardColumnDTO columnDTO = modelMapper.map(boardColumnVO, BoardColumnDTO.class);
            columnDTO.setOrganizationId(organizationId);
            columnDTO.setProjectId(0L);
            boardColumnService.createBase(columnDTO);
            return boardColumnVO;
        }
    }

    @Override
    public BoardColumnVO updateBoardColumn(Long organizationId, Long columnId, Long boardId, BoardColumnVO boardColumnVO) {
        BoardColumnDTO boardColumnDTO = modelMapper.map(boardColumnVO, BoardColumnDTO.class);
        boardColumnDTO.setOrganizationId(organizationId);
        boardColumnDTO.setProjectId(0L);
        if (boardColumnMapper.updateByPrimaryKeySelective(boardColumnDTO) != 1) {
            throw new CommonException("error.board.template.column.update");
        }
        return modelMapper.map(boardColumnMapper.selectByPrimaryKey(boardColumnDTO.getColumnId()), BoardColumnVO.class);
    }

    @Override
    public BoardColumnVO updateColumnContraintTemplate(Long organizationId, Long columnId, ColumnWithMaxMinNumVO columnWithMaxMinNumVO) {
        try {
            boardColumnMapper.updateMaxAndMinNumTemplate(organizationId, columnWithMaxMinNumVO);
        } catch (Exception e) {
            throw new CommonException("error.update.column.contraint", e);
        }
        return modelMapper.map(boardColumnMapper.selectByPrimaryKey(columnWithMaxMinNumVO.getColumnId()), BoardColumnVO.class);
    }

    @Override
    public StatusVO moveStatusToUnCorrespond(Long organizationId, Long statusId, StatusMoveVO statusMoveVO) {
        ColumnStatusRelDTO columnStatusRelDTO = new ColumnStatusRelDTO();
        columnStatusRelDTO.setStatusId(statusId);
        columnStatusRelDTO.setColumnId(statusMoveVO.getColumnId());
        columnStatusRelDTO.setProjectId(0L);
        columnStatusRelDTO.setOrganizationId(organizationId);
        columnStatusRelService.delete(columnStatusRelDTO);
        return modelMapper.map(statusService.queryStatusById(organizationId, statusId), StatusVO.class);
    }

    @Override
    public StatusTemplateVO settingStatusTemplate(Long organizationId, Long statusId, Boolean completed) {
        StatusTemplateDTO statusTemplateDTO = new StatusTemplateDTO(organizationId, statusId);
        List<StatusTemplateDTO> statusTemplates = statusTemplateMapper.select(statusTemplateDTO);
        if (org.springframework.util.CollectionUtils.isEmpty(statusTemplates)) {
            statusTemplateDTO.setTemplateCompleted(completed);
            if (statusTemplateMapper.insertSelective(statusTemplateDTO) != 1) {
                throw new CommonException("error.status.template.insert");
            }
        } else {
            StatusTemplateDTO templateDTO = statusTemplates.get(0);
            templateDTO.setTemplateCompleted(completed);
            if (statusTemplateMapper.updateByPrimaryKeySelective(statusTemplateDTO) != 1) {
                throw new CommonException("error.status.template.update");
            }
        }
        StatusVO statusVO = statusService.queryStatusById(organizationId, statusId);
        StatusTemplateVO statusTemplateVO = modelMapper.map(statusVO, StatusTemplateVO.class);
        statusTemplateVO.setTemplateCompleted(completed);
        statusTemplateVO.setStatusId(statusId);
        return statusTemplateVO;
    }

    @Override
    public List<BoardColumnVO> listColumnByBoardId(Long organizationId, Long boardId) {
        List<BoardColumnVO> boardColumnVOS = boardColumnMapper.listColumnAndStatusByBoardId(organizationId, boardId);
        if (org.springframework.util.CollectionUtils.isEmpty(boardColumnVOS)) {
            return new ArrayList<>();
        }
        // 排序
        Comparator<StatusTemplateVO> comparator = Comparator.comparing(StatusTemplateVO::getPosition, nullsFirst(naturalOrder()));
        boardColumnVOS.forEach(v -> {
            List<StatusTemplateVO> status = v.getStatus();
            if (!org.springframework.util.CollectionUtils.isEmpty(status)) {
                status.sort(comparator);
            }
        });
        return boardColumnVOS;
    }

    @Override
    public StatusVO moveStatusToColumn(Long organizationId, Long statusId, StatusMoveVO statusMoveVO) {
        // 判断是否在同一列中操作，更新列中position
        Boolean sameRow = statusMoveVO.getColumnId().equals(statusMoveVO.getOriginColumnId());
        issueStatusService.deleteColumnStatusRel(organizationId, 0L, statusId, statusMoveVO.getOriginColumnId());
        issueStatusService.updateColumnPosition(organizationId,0L, statusId, statusMoveVO,sameRow);
        return modelMapper.map(statusService.queryStatusById(organizationId, statusId), StatusVO.class);
    }
}
