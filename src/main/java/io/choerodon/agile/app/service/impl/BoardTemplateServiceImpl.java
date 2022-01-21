package io.choerodon.agile.app.service.impl;

import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.api.vo.event.ProjectEvent;
import io.choerodon.agile.api.vo.event.StatusPayload;
import io.choerodon.agile.app.service.*;
import io.choerodon.agile.infra.dto.*;
import io.choerodon.agile.infra.enums.SchemeApplyType;
import io.choerodon.agile.infra.enums.SchemeType;
import io.choerodon.agile.infra.feign.BaseFeignClient;
import io.choerodon.agile.infra.mapper.*;
import io.choerodon.agile.infra.utils.ConvertUtil;
import io.choerodon.agile.infra.utils.PageUtil;
import io.choerodon.core.domain.Page;
import io.choerodon.core.exception.CommonException;
import io.choerodon.mybatis.pagehelper.PageHelper;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;
import org.apache.commons.collections4.CollectionUtils;
import org.hzero.mybatis.domian.Condition;
import org.hzero.mybatis.util.Sqls;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.ObjectUtils;

import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

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

    @Autowired
    private OrganizationConfigMapper organizationConfigMapper;

    @Autowired
    private StateMachineSchemeConfigService stateMachineSchemeConfigService;

    @Autowired
    private IssueTypeService issueTypeService;

    @Autowired
    private ColumnStatusRelMapper columnStatusRelMapper;

    @Autowired
    private BoardColumnTemplateMapper boardColumnTemplateMapper;

    @Autowired
    private ColumnStatusRelTemplateMapper columnStatusRelTemplateMapper;

    @Autowired
    private BaseFeignClient baseFeignClient;

    @Autowired
    private StatusMachineMapper statusMachineMapper;

    @Autowired
    private ProjectConfigMapper projectConfigMapper;

    @Override
    public void createBoardTemplate(Long organizationId, String boardName) {
        if (Boolean.TRUE.equals(boardService.checkName(organizationId, 0L, boardName))) {
            throw new CommonException("error.board.template.name.exist");
        }
        BoardDTO boardResult = boardService.createBoard(organizationId, 0L, boardName, "agile");
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
        createBoardTemplateColumn(organizationId,0L, boardId, TODO, TODO_CODE, SEQUENCE_ONE, COLUMN_COLOR_TODO);
        createBoardTemplateColumn(organizationId,0L, boardId, DOING, DOING_CODE, SEQUENCE_TWO, COLUMN_COLOR_DOING);
        createBoardTemplateColumn(organizationId,0L, boardId, DONE, DONE_CODE, SEQUENCE_THREE, COLUMN_COLOR_DONE);
    }

    private void createBoardTemplateColumn(Long organizationId, Long projectId, Long boardId, String name, String categoryCode, Integer sequence, String colorCode) {
        BoardColumnDTO column = new BoardColumnDTO();
        column.setBoardId(boardId);
        column.setName(name);
        column.setProjectId(projectId);
        column.setOrganizationId(organizationId);
        column.setCategoryCode(categoryCode);
        column.setSequence(sequence);
        column.setColorCode(colorCode);
        boardColumnService.createBase(column);
        List<StatusVO> statusVOS = listUnCorrespondStatusTemplate(organizationId, boardId);
        if (CollectionUtils.isNotEmpty(statusVOS)) {
            List<StatusVO> list = statusVOS.stream()
                    .filter(v -> Objects.equals(categoryCode, v.getType()))
                    .collect(Collectors.toList());
            int position = 0;
            for (StatusVO statusVO : list) {
                ColumnStatusRelDTO columnStatusRelDTO = new ColumnStatusRelDTO();
                columnStatusRelDTO.setColumnId(column.getColumnId());
                columnStatusRelDTO.setStatusId(statusVO.getId());
                columnStatusRelDTO.setProjectId(projectId);
                columnStatusRelDTO.setOrganizationId(organizationId);
                if (columnStatusRelMapper.select(columnStatusRelDTO).isEmpty()) {
                    ColumnStatusRelDTO columnStatusRel = new ColumnStatusRelDTO();
                    columnStatusRel.setColumnId(column.getColumnId());
                    columnStatusRel.setPosition(position++);
                    columnStatusRel.setStatusId(statusVO.getId());
                    columnStatusRel.setProjectId(projectId);
                    columnStatusRel.setOrganizationId(organizationId);
                    columnStatusRelService.create(columnStatusRel);
                }
            }
        }
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
    public Page<BoardVO> listBoardTemplate(Long organizationId, PageRequest pageRequest) {
        BoardDTO boardDTO = new BoardDTO();
        boardDTO.setProjectId(0L);
        boardDTO.setOrganizationId(organizationId);
        Page<BoardDTO> page = PageHelper.doPageAndSort(pageRequest, () -> boardMapper.select(boardDTO));
        List<BoardDTO> boardDTOS = page.getContent();
        if (CollectionUtils.isEmpty(boardDTOS)) {
            return new Page<>();
        }
        List<Long> userIds = boardDTOS.stream().map(BoardDTO::getCreatedBy).collect(Collectors.toList());
        List<UserDTO> userDTOS = baseFeignClient.listUsersByIds(userIds.toArray(new Long[userIds.size()]), true).getBody();
        Map<Long, UserDTO> userDTOMap = new HashMap<>();
        if (CollectionUtils.isNotEmpty(userDTOS)) {
            userDTOMap.putAll(userDTOS.stream().collect(Collectors.toMap(UserDTO::getId, Function.identity())));
        }
        List<BoardVO> boardVOList = new ArrayList<>();
        boardDTOS.forEach(boardTemplate -> {
            BoardVO boardVO = modelMapper.map(boardTemplate, BoardVO.class);
            boardVO.setCreator(userDTOMap.getOrDefault(boardTemplate.getCreatedBy(), null));
            boardVOList.add(boardVO);
        });

        return PageUtil.buildPageInfoWithPageInfoList(page, boardVOList);
    }

    @Override
    public BoardColumnVO createBoardColumn(Long organizationId, String categoryCode, BoardColumnVO boardColumnVO) {
        // 创建列
        boardColumnService.createCheck(boardColumnVO);
        boardColumnService.setColumnColor(boardColumnVO, true);
        BoardColumnDTO columnDTO = modelMapper.map(boardColumnVO, BoardColumnDTO.class);
        columnDTO.setOrganizationId(organizationId);
        columnDTO.setProjectId(0L);
        boardColumnService.createBase(columnDTO);
        return boardColumnVO;
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
            boardColumnTemplateMapper.updateMaxAndMinNumTemplate(organizationId, columnWithMaxMinNumVO);
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
        if (CollectionUtils.isEmpty(statusTemplates)) {
            statusTemplateDTO.setTemplateCompleted(completed);
            if (statusTemplateMapper.insertSelective(statusTemplateDTO) != 1) {
                throw new CommonException("error.status.template.insert");
            }
        } else {
            StatusTemplateDTO templateDTO = statusTemplates.get(0);
            templateDTO.setTemplateCompleted(completed);
            if (statusTemplateMapper.updateByPrimaryKeySelective(templateDTO) != 1) {
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
        List<BoardColumnVO> boardColumnVOS = boardColumnTemplateMapper.listColumnAndStatusByBoardTemplateId(organizationId, boardId);
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

    @Override
    public void syncBoardTemplate(ProjectEvent projectEvent, String applyType) {
        List<BoardVO> boardVOList = boardService.queryByProjectId(projectEvent.getProjectId(), applyType);
        if (CollectionUtils.isNotEmpty(boardVOList)) {
            return;
        }
        // 查询组织的看板
        Long projectId = projectEvent.getProjectId();
        Long organizationId = ConvertUtil.getOrganizationId(projectEvent.getProjectId());
        List<BoardVO> boardVOS = listBoardTemplate(organizationId, new PageRequest(0,0)).getContent();
        if (!CollectionUtils.isEmpty(boardVOS)) {
           // 复制看板模板
           copyBoardTemplate(projectId, organizationId, boardVOS);
        } else {
            Long stateMachineId = queryDefaultStatusMachineId(organizationId, projectId);
            List<StatusPayload> statusPayloads = statusMachineMapper.getStatusBySmId(projectId, stateMachineId);
            boardService.initBoard(projectId, "默认看板", statusPayloads);
        }
    }

    private Long queryDefaultStatusMachineId(Long organizationId, Long projectId) {
        ProjectConfigDTO configDTO = projectConfigMapper.queryBySchemeTypeAndApplyType(projectId, SchemeType.STATE_MACHINE, SchemeApplyType.AGILE);
        if (ObjectUtils.isEmpty(configDTO)) {
            throw new CommonException("error.project.config.null");
        }
        return stateMachineSchemeConfigService.queryStatusMachineBySchemeIdAndIssueType(organizationId, configDTO.getSchemeId(), 0L);
    }

    @Override
    public List<StatusVO> listUnCorrespondStatusTemplate(Long organizationId, Long boardTemplateId) {
        // 查询组织状态机模板的状态
        OrganizationConfigDTO organizationConfigDTO = new OrganizationConfigDTO();
        organizationConfigDTO.setOrganizationId(organizationId);
        OrganizationConfigDTO configDTO = organizationConfigMapper.selectOne(organizationConfigDTO);
        List<StatusVO> statusVOS = new ArrayList<>();
        if (ObjectUtils.isEmpty(configDTO)) {
            List<StatusVO> statusS = statusService.queryAllStatus(organizationId);
            String[] codes = {"create", "processing", "complete"};
            List<String> list = Arrays.asList(codes);
            statusVOS.addAll(statusS.stream().filter(v -> list.contains(v.getCode())).collect(Collectors.toList()));
        } else {
            List<StatusMachineSchemeConfigVO> statusMachineSchemeConfigVOS = stateMachineSchemeConfigService.queryBySchemeId(false, organizationId, configDTO.getSchemeId());
            if (!CollectionUtils.isEmpty(statusMachineSchemeConfigVOS)) {
                // 查询预定义问题类型的状态机模板状态
                List<IssueTypeVO> issueTypeVOS = issueTypeService.queryByOrgId(organizationId, null);
                String[] issueTypeCodes = {"feature", "backlog"};
                List<Long> issueTypeIds = issueTypeVOS.stream()
                        .filter(v -> Objects.equals("system", v.getSource()) && !Arrays.asList(issueTypeCodes).contains(v.getTypeCode()))
                        .map(IssueTypeVO::getId).collect(Collectors.toList());
                List<Long> statusMachineIds = statusMachineSchemeConfigVOS.stream()
                        .filter(v -> issueTypeIds.contains(v.getIssueTypeId()))
                        .map(StatusMachineSchemeConfigVO::getStateMachineId)
                        .collect(Collectors.toList());
                statusVOS.addAll(statusService.queryByStateMachineIds(organizationId, statusMachineIds));
            }
        }
        // 查询看板已有状态
        if (!ObjectUtils.isEmpty(boardTemplateId)) {
            List<Long> statusIds = columnStatusRelTemplateMapper.queryBoardTemplateStatusIds(organizationId, 0L, boardTemplateId);
            if (!CollectionUtils.isEmpty(statusIds)) {
                statusVOS = statusVOS.stream()
                        .filter(v -> !statusIds.contains(v.getId()))
                        .collect(Collectors.toList());
            }
        }
        Map<Long, Boolean> maps = new HashMap<>();
        List<Long> status = statusVOS.stream().map(StatusVO::getId).collect(Collectors.toList());
        if (!CollectionUtils.isEmpty(status)) {
            Sqls existCondition = Sqls.custom().andEqualTo("organizationId", organizationId).andIn("statusId",status);
            List<StatusTemplateDTO> statusTemplateDTOS = statusTemplateMapper.selectByCondition(Condition.builder(StatusTemplateDTO.class)
                    .andWhere(existCondition).build());
            if (CollectionUtils.isNotEmpty(statusTemplateDTOS)) {
                maps = statusTemplateDTOS.stream()
                        .collect(Collectors.toMap(StatusTemplateDTO::getStatusId, StatusTemplateDTO::getTemplateCompleted));
            }
        }
        for (StatusVO statusVO : statusVOS) {
            Boolean isCompleted = maps.get(statusVO.getId());
            statusVO.setCompleted(ObjectUtils.isEmpty(isCompleted) ? Boolean.FALSE : isCompleted);
        }
        return statusVOS;
    }

    @Override
    public void deleteBoardTemplateColumn(Long organizationId, Long templateColumnId) {
        BoardColumnDTO boardColumn = new BoardColumnDTO();
        boardColumn.setOrganizationId(organizationId);
        boardColumn.setProjectId(0L);
        boardColumn.setColumnId(templateColumnId);
        BoardColumnDTO boardColumnDTO = boardColumnMapper.selectOne(boardColumn);
        if (ObjectUtils.isEmpty(boardColumnDTO)) {
            throw new CommonException("error.board.template.column.null");
        }
        // 删除列
        if (boardColumnMapper.deleteByPrimaryKey(templateColumnId) != 1) {
            throw new CommonException("error.board.template.column.delete");
        }
        // 取消列下的状态关联，状态归为未对应的状态
        ColumnStatusRelDTO columnStatusRelDTO = new ColumnStatusRelDTO();
        columnStatusRelDTO.setColumnId(templateColumnId);
        columnStatusRelDTO.setProjectId(0L);
        columnStatusRelDTO.setOrganizationId(organizationId);
        columnStatusRelService.delete(columnStatusRelDTO);
        updateSequenceWhenDeleteTemplateColumn(organizationId, 0L, boardColumnDTO);
    }

    @Override
    public void deleteBoardTemplateStatus(Long currentStatusId, Long organizationId) {
        List<StatusVO> statusVOS = listUnCorrespondStatusTemplate(organizationId, null);
        if (CollectionUtils.isNotEmpty(statusVOS)) {
            List<Long> statusIds = statusVOS.stream().map(StatusVO::getId).collect(Collectors.toList());
            if (!statusIds.contains(currentStatusId)) {
                ColumnStatusRelDTO columnStatusRelDTO = new ColumnStatusRelDTO();
                columnStatusRelDTO.setOrganizationId(organizationId);
                columnStatusRelDTO.setStatusId(currentStatusId);
                columnStatusRelDTO.setProjectId(0L);
                columnStatusRelMapper.delete(columnStatusRelDTO);
            }
        }
    }

    public void updateSequenceWhenDeleteTemplateColumn(Long organizationId, Long projectId, BoardColumnDTO boardColumnDTO) {
        Long boardId = boardColumnDTO.getBoardId();
        boardColumnMapper.updateSequenceWhenDelete(boardColumnDTO.getBoardId(), boardColumnDTO.getSequence());
        BoardColumnDTO update = new BoardColumnDTO();
        update.setProjectId(projectId);
        update.setOrganizationId(organizationId);
        update.setBoardId(boardId);
        Integer size = boardColumnMapper.select(update).size();
        boardColumnMapper.updateColumnCategory(boardId, size);
        boardColumnMapper.updateColumnColor(boardId, size);
    }

    private void copyBoardTemplate(Long projectId, Long organizationId, List<BoardVO> boardVOS) {
        for (BoardVO boardVO : boardVOS) {
            BoardDTO board = boardService.createBoard(0L, projectId, boardVO.getName(), "agile");
            // 查询列
            List<BoardColumnVO> boardColumnVOS = listColumnByBoardId(organizationId, boardVO.getBoardId());
            if (!CollectionUtils.isEmpty(boardColumnVOS)) {
                for (BoardColumnVO boardColumnVO : boardColumnVOS) {
                    BoardColumnDTO columnDTO = modelMapper.map(boardColumnVO, BoardColumnDTO.class);
                    columnDTO.setOrganizationId(0L);
                    columnDTO.setProjectId(projectId);
                    columnDTO.setColumnId(null);
                    columnDTO.setObjectVersionNumber(null);
                    columnDTO.setBoardId(board.getBoardId());
                    BoardColumnDTO boardColumnDTO = boardColumnService.createBase(columnDTO);
                    // 关联状态
                    List<StatusTemplateVO> status = boardColumnVO.getStatus();
                    for (StatusTemplateVO statusTemplateVO : status) {
                        ColumnStatusRelDTO columnStatusRelDTO = new ColumnStatusRelDTO();
                        columnStatusRelDTO.setOrganizationId(0L);
                        columnStatusRelDTO.setProjectId(projectId);
                        columnStatusRelDTO.setPosition(statusTemplateVO.getPosition());
                        columnStatusRelDTO.setStatusId(statusTemplateVO.getStatusId());
                        columnStatusRelDTO.setColumnId(boardColumnDTO.getColumnId());
                        columnStatusRelService.create(columnStatusRelDTO);
                    }
                }
            }
        }
    }
}
