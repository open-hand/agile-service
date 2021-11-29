package io.choerodon.agile.app.service.impl;

import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONObject;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import io.choerodon.core.domain.Page;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.hzero.core.base.BaseConstants;
import org.modelmapper.ModelMapper;
import org.modelmapper.TypeToken;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.ObjectUtils;

import java.io.IOException;
import java.util.*;
import java.util.stream.Collectors;

import static java.util.Comparator.naturalOrder;
import static java.util.Comparator.nullsFirst;

import io.choerodon.agile.api.validator.BoardValidator;
import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.api.vo.business.IssueVO;
import io.choerodon.agile.api.vo.event.StatusPayload;
import io.choerodon.agile.app.assembler.BoardAssembler;
import io.choerodon.agile.app.service.*;
import io.choerodon.agile.infra.dto.*;
import io.choerodon.agile.infra.dto.business.IssueDTO;
import io.choerodon.agile.infra.enums.SchemeApplyType;
import io.choerodon.agile.infra.mapper.*;
import io.choerodon.agile.infra.statemachineclient.dto.InputDTO;
import io.choerodon.agile.infra.utils.*;
import io.choerodon.core.exception.CommonException;
import io.choerodon.core.oauth.CustomUserDetails;
import io.choerodon.core.oauth.DetailsHelper;

/**
 * Created by HuangFuqiang@choerodon.io on 2018/5/14.
 * Email: fuqianghuang01@gmail.com
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class BoardServiceImpl implements BoardService {

    private static final String CONTRAINT_NONE = "constraint_none";
    private static final String STORY_POINTS = "story_point";
    private static final String PARENT_CHILD = "parent_child";
    private static final String BOARD = "board";
    private static final String PROJECT_ID = "projectId";
    private static final String RANK = "rank";
    private static final String UPDATE_STATUS_MOVE = "updateStatusMove";
    private static final String SPRINT_FIELD = "sprint";
    @Autowired
    private BoardMapper boardMapper;
    @Autowired
    private BoardColumnService boardColumnService;
    @Autowired
    private BoardColumnMapper boardColumnMapper;
    @Autowired
    private SprintService sprintService;
    @Autowired
    private IssueMapper issueMapper;
    @Autowired
    private UserService userService;
    @Autowired
    private QuickFilterMapper quickFilterMapper;
    @Autowired
    private UserSettingMapper userSettingMapper;
    @Autowired
    private UserSettingService userSettingService;
    @Autowired
    private DateUtil dateUtil;
    @Autowired
    private WorkCalendarRefMapper workCalendarRefMapper;
    @Autowired
    private StateMachineClientService stateMachineClientService;
    @Autowired
    private SprintMapper sprintMapper;
    @Autowired
    private SendMsgUtil sendMsgUtil;
    @Autowired
    private ColumnStatusRelService columnStatusRelService;
    @Autowired
    private PriorityService priorityService;
    @Autowired
    private IssueTypeService issueTypeService;
    @Autowired
    private StatusService statusService;
    @Autowired
    private ModelMapper modelMapper;
    @Autowired
    private PersonalFilterMapper personalFilterMapper;
    @Autowired
    private BoardAssembler boardAssembler;
    @Autowired
    private StatusFieldSettingService statusFieldSettingService;
    @Autowired
    private StatusLinkageService statusLinkageService;
    @Autowired(required = false)
    private BacklogExpandService backlogExpandService;
    @Autowired
    private StarBeaconMapper starBeaconMapper;
    @Autowired
    private ObjectMapper objectMapper;
    @Autowired
    private LinkIssueStatusLinkageService linkIssueStatusLinkageService;
    @Autowired
    private IssueService issueService;
    @Autowired
    private IssueParticipantRelMapper issueParticipantRelMapper;
    @Autowired
    private ProjectInfoMapper projectInfoMapper;
    @Autowired
    private QuickFilterService quickFilterService;
    @Autowired
    private BoardQuickFilterRelMapper boardQuickFilterRelMapper;

    @Override
    public void create(Long projectId, String boardName) {
        if (Boolean.TRUE.equals(checkName(0L, projectId, boardName))) {
            throw new CommonException("error.boardName.exist");
        }
        BoardDTO boardResult = createBoard(0L, projectId, boardName);
        boardColumnService.createColumnWithRelateStatus(boardResult);
    }

    private Boolean checkNameUpdate(Long projectId, Long boardId, String boardName) {
        BoardDTO boardDTO = boardMapper.selectByPrimaryKey(boardId);
        if (boardName.equals(boardDTO.getName())) {
            return false;
        }
        if (!projectId.equals(boardDTO.getProjectId())) {
            throw new CommonException("error.project.id.illegal");
        }
        BoardDTO check = new BoardDTO();
        check.setProjectId(projectId);
        check.setName(boardName);
        List<BoardDTO> boardDTOList = boardMapper.select(check);
        return boardDTOList != null && !boardDTOList.isEmpty();
    }

    @Override
    public BoardVO update(Long projectId, Long boardId, BoardVO boardVO) {
        if (boardVO.getName() != null && checkNameUpdate(projectId, boardId, boardVO.getName())) {
            throw new CommonException("error.boardName.exist");
        }
        BoardValidator.checkUpdateBoard(projectId, boardVO);
        boardVO.setBoardId(boardId);
        if (boardMapper.updateByPrimaryKeySelective(modelMapper.map(boardVO, BoardDTO.class)) != 1) {
            throw new CommonException("error.board.update");
        }
        return modelMapper.map(boardMapper.selectByPrimaryKey(boardVO.getBoardId()), BoardVO.class);
    }

    @Override
    public void delete(Long projectId, Long boardId) {
        BoardDTO boardDTO = new BoardDTO();
        boardDTO.setProjectId(projectId);
        List<BoardDTO> boardDTOS = boardMapper.select(boardDTO);
        BoardValidator.checkBoardUnique(boardDTOS);

        BoardColumnDTO boardColumnDTO = new BoardColumnDTO();
        boardColumnDTO.setBoardId(boardId);
        List<BoardColumnDTO> boardColumnDTOList = boardColumnMapper.select(boardColumnDTO);
        for (BoardColumnDTO column : boardColumnDTOList) {
            ColumnStatusRelDTO columnStatusRelDTO = new ColumnStatusRelDTO();
            columnStatusRelDTO.setColumnId(column.getColumnId());
            columnStatusRelDTO.setProjectId(projectId);
            columnStatusRelService.delete(columnStatusRelDTO);
            if (boardColumnMapper.deleteByPrimaryKey(column.getColumnId()) != 1) {
                throw new CommonException("error.BoardColumn.delete");
            }
        }
        if (boardMapper.deleteByPrimaryKey(boardId) != 1) {
            throw new CommonException("error.board.delete");
        }
        //删除默认看板UserSetting
        UserSettingDTO userSettingDTO = new UserSettingDTO();
        userSettingDTO.setProjectId(projectId);
        userSettingDTO.setTypeCode(BOARD);
        userSettingDTO.setBoardId(boardId);
        userSettingDTO.setUserId(DetailsHelper.getUserDetails().getUserId());
        userSettingMapper.delete(userSettingDTO);
        //更新第一个为默认
        List<BoardVO> boardVOS = queryByProjectId(projectId);
        if (!boardVOS.isEmpty()) {
            Long defaultBoardId = boardVOS.get(0).getBoardId();
            handleUserSetting(defaultBoardId, projectId);
        }
    }

    @Override
    public BoardVO queryScrumBoardById(Long projectId, Long boardId) {
        BoardDTO boardDTO = new BoardDTO();
        boardDTO.setProjectId(projectId);
        boardDTO.setBoardId(boardId);
        return modelMapper.map(boardMapper.selectOne(boardDTO), BoardVO.class);
    }

    public JSONObject putColumnData(List<ColumnAndIssueDTO> columns) {
        JSONObject columnsData = new JSONObject();
        columnsData.put("columns", columns);
        return columnsData;
    }

    private void addIssueInfos(IssueForBoardDO issue, List<Long> parentIds, List<Long> assigneeIds, List<Long> ids, List<Long> epicIds, Map<Long, PriorityVO> priorityMap, Map<Long, IssueTypeVO> issueTypeDTOMap, Map<Long, List<Long>> parentWithSubs) {
        if (issue.getParentIssueId() != null && issue.getParentIssueId() != 0 && !parentIds.contains(issue.getParentIssueId())) {
            parentIds.add(issue.getParentIssueId());
        } else if (issue.getRelateIssueId() != null && issue.getRelateIssueId() != 0 && !parentIds.contains(issue.getRelateIssueId())) {
            parentIds.add(issue.getRelateIssueId());
        } else {
            ids.add(issue.getIssueId());
        }
        if (issue.getAssigneeId() != null && !assigneeIds.contains(issue.getAssigneeId())) {
            assigneeIds.add(issue.getAssigneeId());
        }
        if (issue.getEpicId() != null && !epicIds.contains(issue.getEpicId())) {
            epicIds.add(issue.getEpicId());
        }
        if ("sub_task".equals(issue.getTypeCode()) && issue.getParentIssueId() != null) {
            List<Long> subtaskIds = parentWithSubs.getOrDefault(issue.getParentIssueId(), new ArrayList<>());
            subtaskIds.add(issue.getIssueId());
            parentWithSubs.put(issue.getParentIssueId(), subtaskIds);
        }
        if ("bug".equals(issue.getTypeCode()) && issue.getRelateIssueId() != null) {
            List<Long> subBugIds = parentWithSubs.getOrDefault(issue.getRelateIssueId(), new ArrayList<>());
            subBugIds.add(issue.getIssueId());
            parentWithSubs.put(issue.getRelateIssueId(), subBugIds);
        }
        issue.setPriorityVO(priorityMap.get(issue.getPriorityId()));
        issue.setIssueTypeVO(issueTypeDTOMap.get(issue.getIssueTypeId()));
        if (issue.getStayDate() != null) {
            issue.setStayDay(DateUtil.differentDaysByMillisecond(issue.getStayDate(), new Date()));
        } else {
            issue.setStayDay(0);
        }
    }

    private void getDatas(List<SubStatusDTO> subStatusDTOS, List<Long> parentIds, List<Long> assigneeIds, List<Long> ids, List<Long> epicIds, Long organizationId, Map<Long, List<Long>> parentWithSubs, Map<Long, IssueTypeVO> issueTypeDTOMap) {
        Map<Long, PriorityVO> priorityMap = priorityService.queryByOrganizationId(organizationId);
        subStatusDTOS.forEach(subStatusDTO -> subStatusDTO.getIssues().forEach(issueForBoardDO -> addIssueInfos(issueForBoardDO, parentIds, assigneeIds, ids, epicIds, priorityMap, issueTypeDTOMap, parentWithSubs)));
    }

    public void putDatasAndSort(List<ColumnAndIssueDTO> columns, List<Long> parentIds, List<Long> assigneeIds, Long boardId, List<Long> epicIds, Boolean condition, Long organizationId, Map<Long, List<Long>> parentWithSubss, Map<Long, StatusVO> statusMap, Map<Long, IssueTypeVO> issueTypeDTOMap) {
        List<Long> issueIds = new ArrayList<>();
        for (ColumnAndIssueDTO column : columns) {
            List<SubStatusDTO> subStatusDTOS = column.getSubStatusDTOS();
            subStatusDTOS = fillStatusData(subStatusDTOS, statusMap);
            getDatas(subStatusDTOS, parentIds, assigneeIds, issueIds, epicIds, organizationId, parentWithSubss, issueTypeDTOMap);
            Collections.sort(subStatusDTOS, (o1, o2) -> o1.getPosition() - o2.getPosition());
            column.setSubStatusDTOS(subStatusDTOS);
        }
        //选择故事泳道选择仅我的任务后，子任务经办人为自己，父任务经办人不为自己的情况
        if (Boolean.TRUE.equals(condition)) {
            handleParentIdsWithSubIssues(parentIds, issueIds, columns, boardId);
        }
        Collections.sort(parentIds);
        Collections.sort(assigneeIds);
    }

    private List<SubStatusDTO> fillStatusData(List<SubStatusDTO> subStatusDTOS, Map<Long, StatusVO> statusMap) {
        List<SubStatusDTO> subStatusDTO1 = new ArrayList<>();
        for (SubStatusDTO subStatusDTO : subStatusDTOS) {
            StatusVO status = statusMap.get(subStatusDTO.getStatusId());
            if (!ObjectUtils.isEmpty(status)) {
                subStatusDTO.setCategoryCode(status.getType());
                subStatusDTO.setName(status.getName());
                Collections.sort(subStatusDTO.getIssues(), Comparator.comparing(IssueForBoardDO::getIssueId));
                subStatusDTO1.add(subStatusDTO);
            }
        }
        return subStatusDTO1;
    }

    private void handleParentIdsWithSubIssues(List<Long> parentIds, List<Long> issueIds, List<ColumnAndIssueDTO> columns, Long boardId) {
        if (parentIds != null && !parentIds.isEmpty()) {
            List<Long> subNoParentIds = new ArrayList<>();
            parentIds.forEach(id -> {
                if (!issueIds.contains(id)) {
                    subNoParentIds.add(id);
                }
            });
            if (!subNoParentIds.isEmpty()) {
                List<ColumnAndIssueDTO> subNoParentColumns = boardColumnMapper.queryColumnsByIssueIds(subNoParentIds, boardId);
                subNoParentColumns.forEach(columnAndIssueDTO -> handleSameColumn(columns, columnAndIssueDTO));
            }
        }
    }

    private void handleSameColumn(List<ColumnAndIssueDTO> columns, ColumnAndIssueDTO columnAndIssueDTO) {
        Optional<ColumnAndIssueDTO> sameColumn = columns.stream().filter(columnAndIssue -> columnAndIssue.getColumnId().equals(columnAndIssueDTO.getColumnId()))
                .findFirst();
        if (sameColumn.isPresent()) {
            sameColumn.get().getSubStatusDTOS().forEach(subStatusDTO -> columnAndIssueDTO.getSubStatusDTOS().forEach(s -> {
                if (subStatusDTO.getId().equals(s.getId())) {
                    subStatusDTO.getIssues().addAll(s.getIssues());
                }
            }));
        } else {
            columns.add(columnAndIssueDTO);
        }
    }


    private SprintDTO getActiveSprint(Long projectId) {
        return sprintService.getActiveSprint(projectId);
    }

    private BoardSprintVO putCurrentSprint(SprintDTO activeSprint, Long organizationId) {
        if (activeSprint != null) {
            BoardSprintVO boardSprintVO = new BoardSprintVO();
            boardSprintVO.setSprintId(activeSprint.getSprintId());
            boardSprintVO.setSprintName(activeSprint.getSprintName());
            if (activeSprint.getEndDate() != null) {
                Date startDate = new Date();
                if (activeSprint.getStartDate().after(startDate)) {
                    startDate = activeSprint.getStartDate();
                }
                boardSprintVO.setDayRemain(dateUtil.getDaysBetweenDifferentDate(startDate, activeSprint.getEndDate(),
                        workCalendarRefMapper.queryHolidayBySprintIdAndProjectId(activeSprint.getSprintId(), activeSprint.getProjectId()),
                        workCalendarRefMapper.queryWorkBySprintIdAndProjectId(activeSprint.getSprintId(), activeSprint.getProjectId()), organizationId));
            }
            return boardSprintVO;
        }
        return null;
    }

    @Override
    public String getQuickFilter(List<Long> quickFilterIds) {
        List<String> sqlQuerys = quickFilterMapper.selectSqlQueryByIds(quickFilterIds);
        if (sqlQuerys.isEmpty()) {
            return null;
        }
        StringBuilder sql = new StringBuilder("select issue_id from agile_issue where ");
        int idx = 0;
        for (String filter : sqlQuerys) {
            if (idx == 0) {
                sql.append(" ( " + filter + " ) ");
                idx += 1;
            } else {
                sql.append(" and " + " ( " + filter + " ) ");
            }
        }
        return sql.toString();
    }

    private List<Long> sortAndJudgeCompleted(Long projectId, List<Long> parentIds) {
        if (parentIds != null && !parentIds.isEmpty()) {
            return boardColumnMapper.sortAndJudgeCompleted(projectId, parentIds);
        } else {
            return new ArrayList<>();
        }
    }

    private List<ParentIssueDTO> getParentIssues(Long projectId, List<Long> parentIds, Map<Long, StatusVO> statusMap, Map<Long, IssueTypeVO> issueTypeDTOMap) {
        if (parentIds == null || parentIds.isEmpty()) {
            return new ArrayList<>();
        }
        List<ParentIssueDTO> parentIssueDTOList = boardColumnMapper.queryParentIssuesByIds(projectId, parentIds);
        for (ParentIssueDTO parentIssueDTO : parentIssueDTOList) {
            parentIssueDTO.setStatusVO(statusMap.get(parentIssueDTO.getStatusId()));
            parentIssueDTO.setIssueTypeVO(issueTypeDTOMap.get(parentIssueDTO.getIssueTypeId()));
        }
        return parentIssueDTOList;
    }

    private List<ColumnIssueNumDTO> getAllColumnNum(List<ColumnAndIssueDTO> columns, Long projectId, Long boardId, Long sprintId) {
        BoardDTO boardDTO = boardMapper.selectByPrimaryKey(boardId);
        if (CONTRAINT_NONE.equals(boardDTO.getColumnConstraint())) {
            return new ArrayList<>();
        }
        Map<Long, ColumnIssueNumDTO> statusCountMap = new HashMap<>(columns.size() * 2);
        List<ColumnIssueNumDTO> columnIssueNumList = new ArrayList<>();
        Set<Long> statusIds = new HashSet<>();
        columns.forEach(column -> {
            ColumnIssueNumDTO columnIssueNum = new ColumnIssueNumDTO();
            columnIssueNum.setColumnId(column.getColumnId());
            columnIssueNum.setIssueCount(0L);
            columnIssueNumList.add(columnIssueNum);
            if (!CollectionUtils.isEmpty(column.getSubStatusDTOS())) {
                column.getSubStatusDTOS().forEach(status -> {
                    if (status.getStatusId() == null) {
                        return;
                    }
                    statusCountMap.put(status.getStatusId(), columnIssueNum);
                    statusIds.add(status.getStatusId());
                });
            }
        });
        List<IssueCountStatusVO> issueCountList = boardColumnMapper.getColumnNumByStatus(statusIds, projectId, sprintId, boardDTO.getColumnConstraint());
        issueCountList.forEach(issueCount -> {
            ColumnIssueNumDTO columnIssueNumDTO = statusCountMap.get(issueCount.getStatusId());
            if (columnIssueNumDTO == null) {
                return;
            }
            columnIssueNumDTO.setIssueCount(columnIssueNumDTO.getIssueCount() + issueCount.getIssueCount());
        });
        return columnIssueNumList;
    }

    @Override
    public JSONObject queryAllData(Long projectId, Long boardId, Long organizationId, SearchVO searchVO) {
        JSONObject jsonObject = new JSONObject(true);
        //没有传冲刺id，则使用活跃的冲刺
        SprintDTO currentSprint = handlerCurrentSprint(projectId, searchVO);
        String filterSql = getFilterSqlFromSearchVO(searchVO, boardId, projectId);
        boardAssembler.handleOtherArgs(searchVO);
        List<Long> assigneeIds = new ArrayList<>();
        List<Long> parentIds = new ArrayList<>();
        List<Long> epicIds = new ArrayList<>();
        List<Long> participantIds = new ArrayList<>();
        Long userId = DetailsHelper.getUserDetails().getUserId();
        List<ColumnAndIssueDTO> columns = boardColumnMapper.selectColumnInfoByBoardId(projectId, boardId);
        setColumnIssue(columns, projectId, currentSprint.getSprintId(), filterSql, searchVO, searchVO.getAssigneeFilterIds(), userId, participantIds);
        Boolean condition = handlerAssigneeAndStory(searchVO);
        Map<Long, List<Long>> parentWithSubs = new HashMap<>();
        Map<Long, StatusVO> statusMap = statusService.queryAllStatusMap(organizationId);
        Map<Long, IssueTypeVO> issueTypeDTOMap = issueTypeService.listIssueTypeMap(organizationId, projectId);
        putDatasAndSort(columns, parentIds, assigneeIds, boardId, epicIds, condition, organizationId, parentWithSubs, statusMap, issueTypeDTOMap);
        jsonObject.put("parentIds", EncryptionUtils.encryptList(parentIds));
        jsonObject.put("parentIssues", getParentIssues(projectId, parentIds, statusMap, issueTypeDTOMap));
        jsonObject.put("assigneeIds", EncryptionUtils.encryptList(assigneeIds));
        jsonObject.put("participantIds", EncryptionUtils.encryptList(participantIds));
        jsonObject.put("parentWithSubs", EncryptionUtils.encryptMap(parentWithSubs));
        jsonObject.put("parentCompleted", EncryptionUtils.encryptList(sortAndJudgeCompleted(projectId, parentIds)));
        jsonObject.put("epicInfo", !epicIds.isEmpty() ? boardColumnMapper.selectEpicBatchByIds(epicIds) : null);
        jsonObject.put("allColumnNum", getAllColumnNum(columns, projectId, boardId, currentSprint.getSprintId()));
        Map<Long, UserMessageDTO> usersMap = userService.queryUsersMap(assigneeIds, true);
        Comparator<IssueForBoardDO> comparator = Comparator.comparing(IssueForBoardDO::getRank, nullsFirst(naturalOrder()));
        columns.forEach(columnAndIssueDTO ->
            columnAndIssueDTO.getSubStatusDTOS().forEach(subStatusDTO -> {
                subStatusDTO.getIssues().forEach(issueForBoardDO -> {
                    UserMessageDTO userMessageDTO = usersMap.get(issueForBoardDO.getAssigneeId());
                    if(userMessageDTO != null){
                        String assigneeName = userMessageDTO.getName();
                        String assigneeLoginName =  userMessageDTO.getLoginName();
                        String assigneeRealName = userMessageDTO.getRealName();
                        String imageUrl = userMessageDTO.getImageUrl();
                        String email = userMessageDTO.getEmail();
                        boolean ldap = userMessageDTO.getLdap();
                        issueForBoardDO.setAssigneeName(assigneeName);
                        issueForBoardDO.setAssigneeLoginName(assigneeLoginName);
                        issueForBoardDO.setAssigneeRealName(assigneeRealName);
                        issueForBoardDO.setImageUrl(imageUrl);
                        issueForBoardDO.setEmail(email);
                        issueForBoardDO.setLdap(ldap);
                    }
                });
                subStatusDTO.getIssues().sort(comparator);
            }));
        jsonObject.put("columnsData", putColumnData(columns));
        jsonObject.put("currentSprint", putCurrentSprint(currentSprint, organizationId));
        //处理用户默认看板设置，保存最近一次的浏览
        handleUserSetting(boardId, projectId);
        return jsonObject;
    }

    private String getFilterSqlFromSearchVO(SearchVO searchVO,
                                            Long boardId,
                                            Long projectId) {
        List<Long> quickFilterIds = searchVO.getQuickFilterIds();
        Set<Long> defaultQuickFilterIds =
                listQuickFiltersByBoardId(projectId, boardId)
                        .stream()
                        .map(BoardQuickFilterRelVO::getQuickFilterId)
                        .collect(Collectors.toSet());
        if (!ObjectUtils.isEmpty(quickFilterIds)) {
            defaultQuickFilterIds.addAll(quickFilterIds);
        }
        if (!defaultQuickFilterIds.isEmpty()) {
            return getQuickFilter(new ArrayList<>(defaultQuickFilterIds));
        } else {
            return null;
        }
    }

    private void setColumnIssue(List<ColumnAndIssueDTO> columns, Long projectId, Long sprintId, String filterSql, SearchVO searchVO, List<Long> assigneeFilterIds, Long userId, List<Long> participantIds) {
        if (CollectionUtils.isEmpty(columns)){
            return;
        }
        Map<Long, List<IssueForBoardDO>> issueStatusMap = new HashMap<>(columns.size() * 2);
        Set<Long> statusIds = new HashSet<>();
        columns.forEach(column -> {
            if (!CollectionUtils.isEmpty(column.getSubStatusDTOS())){
                column.getSubStatusDTOS().forEach(status -> {
                    status.setIssues(new ArrayList<>());
                    if (status.getStatusId() == null) {
                        return;
                    }
                    statusIds.add(status.getStatusId());
                    issueStatusMap.put(status.getStatusId(), status.getIssues());
                });
            }
        });
        // 项目是否设置隐藏历史迭代中已完成的子任务
        addSearchParam(projectId, sprintId, searchVO);
        List<IssueForBoardDO> issueList = boardColumnMapper.selectBoardIssue(new HashSet<>(Arrays.asList(projectId)), sprintId, filterSql, searchVO, assigneeFilterIds, userId, statusIds);
        if (CollectionUtils.isEmpty(issueList)){
            return;
        }
        List<Long> issueIds = issueList.stream().map(IssueForBoardDO::getIssueId).collect(Collectors.toList());
        List<IssueParticipantRelDTO> issueParticipantRelDTOS = issueParticipantRelMapper.listParticipantByIssueIds(projectId, issueIds);
        Map<Long, List<Long>> participantGroupMap = new HashMap<>();
        Map<Long, UserMessageDTO> userMessageDTOMap = new HashMap<>();
        if (CollectionUtils.isNotEmpty(issueParticipantRelDTOS)) {
            participantGroupMap.putAll(issueParticipantRelDTOS.stream().collect(Collectors.groupingBy(IssueParticipantRelDTO::getIssueId, Collectors.mapping(IssueParticipantRelDTO::getParticipantId, Collectors.toList()))));
            Set<Long> participants = issueParticipantRelDTOS.stream().map(IssueParticipantRelDTO::getParticipantId).collect(Collectors.toSet());
            participantIds.addAll(participants);
            userMessageDTOMap.putAll(userService.queryUsersMap(participantIds, true));
        }
        issueList.forEach(issue -> {
            List<IssueForBoardDO> statusIssueList = issueStatusMap.computeIfAbsent(issue.getStatusId(), k -> new ArrayList<>());
            issue.setStatusId(null);
            statusIssueList.add(issue);
            List<Long> participants = participantGroupMap.get(issue.getIssueId());
            if (CollectionUtils.isNotEmpty(participants)) {
                issue.setParticipantIds(participants);
                issue.setParticipants(participants.stream().map(v -> userMessageDTOMap.get(v)).filter(Objects::nonNull).collect(Collectors.toList()));
            }
        });
        Collections.sort(participantIds);
    }

    private void addSearchParam(Long projectId, Long sprintId,SearchVO searchVO) {
        ProjectInfoDTO projectInfoDTO = projectInfoMapper.queryByProjectId(projectId);
        if (Boolean.TRUE.equals(projectInfoDTO.getHidePreSprintDoneSubissue())) {
            Map<String, Object> searchArgs = searchVO.getSearchArgs();
            if (ObjectUtils.isEmpty(searchArgs)) {
                searchArgs = new HashMap<>();
            }
            searchArgs.put("hidePreSprintDoneSubissue", true);
            searchVO.setSearchArgs(searchArgs);
        }
    }

    private SprintDTO handlerCurrentSprint(Long projectId, SearchVO searchVO) {
        Long sprintId = null;
        if (searchVO.getOtherArgs() != null && searchVO.getOtherArgs().get(SPRINT_FIELD) != null) {
            List<String> sprintIds = (List<String>) searchVO.getOtherArgs().get(SPRINT_FIELD);
            sprintId = Long.valueOf(sprintIds.get(0));
        }
        if (ObjectUtils.isEmpty(sprintId)) {
            SprintDTO activeSprint = getActiveSprint(projectId);
            if (ObjectUtils.isEmpty(activeSprint)) {
               return new SprintDTO();
            }
            Map<String, Object> otherArgs = searchVO.getOtherArgs();
            if (ObjectUtils.isEmpty(otherArgs)) {
                otherArgs = new HashMap<>();
            }
            otherArgs.put(SPRINT_FIELD, Arrays.asList(activeSprint.getSprintId()));
            return activeSprint;
        } else {
            return sprintMapper.selectByPrimaryKey(sprintId);
        }
    }

    private Boolean handlerAssigneeAndStory(SearchVO searchVO) {
        if (ObjectUtils.isEmpty(searchVO)) {
            return false;
        }
        Boolean isAssignee = false;
        if (searchVO.getOtherArgs() != null && searchVO.getOtherArgs().get("assigneeId") != null) {
            List<String> assigneeIds = (List<String>) searchVO.getOtherArgs().get("assigneeId");
            String userId = DetailsHelper.getUserDetails().getUserId().toString();
            isAssignee = assigneeIds.contains(userId);
        }
        Boolean onlyStory = true;
        if (searchVO.getAdvancedSearchArgs() != null && searchVO.getAdvancedSearchArgs().get("issueTypeId") != null) {
            List<String> issueTypeIds = new ArrayList<>();
            try {
                String issueTypeListString = objectMapper.writeValueAsString(searchVO.getAdvancedSearchArgs().get("issueTypeId"));
                issueTypeIds = objectMapper.readValue(issueTypeListString, new TypeReference<List<String>>(){});
            } catch (IOException e) {
                throw new CommonException(e.getMessage(), e);
            }

            for (String issueTypeId : issueTypeIds) {
                String typeCode = issueTypeService.getIssueTypeById(Long.valueOf(issueTypeId));
                if (!Objects.equals(typeCode, "story")) {
                    onlyStory = false;
                    break;
                }
            }
        }
        return isAssignee && onlyStory;
    }

    @Override
    public List<SearchVO> getSearchVO(List<Long> personFilterIds) {
        if (CollectionUtils.isEmpty(personFilterIds)){
            return Collections.emptyList();
        }
        List<PersonalFilterDTO> personalFilterList =
                personalFilterMapper.selectByIds(StringUtils.join(personFilterIds, BaseConstants.Symbol.COMMA));
        return personalFilterList.stream().map(filter -> {
            SearchVO searchVO = JSON.parseObject(filter.getFilterJson(), SearchVO.class);
            boardAssembler.handleOtherArgs(searchVO);
            return searchVO;
        }).collect(Collectors.toList());
    }

    @Override
    public Boolean isLinked(Long projectId, Long issueId, Long statusId) {
        IssueDTO issue = issueMapper.selectByPrimaryKey(issueId);
        if (ObjectUtils.isEmpty(issue)) {
            throw new CommonException("error.issue.not.existed");
        }
        Long issueTypeId = issue.getIssueTypeId();
        return !statusLinkageService.listByIssueTypeAndStatusId(projectId, issueTypeId, statusId).isEmpty();
    }

    private void handleUserSetting(Long boardId, Long projectId) {
        Long userId = DetailsHelper.getUserDetails().getUserId();
        UserSettingDTO userSettingDTO = new UserSettingDTO();
        userSettingDTO.setProjectId(projectId);
        userSettingDTO.setTypeCode(BOARD);
        userSettingDTO.setBoardId(boardId);
        userSettingDTO.setUserId(DetailsHelper.getUserDetails().getUserId());
        UserSettingDTO query = userSettingMapper.selectOne(userSettingDTO);
        if (query == null) {
            userSettingDTO.setDefaultBoard(true);
            userSettingDTO.setSwimlaneBasedCode("swimlane_none");
            int insert = userSettingMapper.insert(userSettingDTO);
            if (insert != 1) {
                throw new CommonException("error.userSetting.create");
            }
            userSettingMapper.updateOtherBoardNoDefault(boardId, projectId, userId);
        } else if (Boolean.FALSE.equals(query.getDefaultBoard())) {
            query.setDefaultBoard(true);
            if (userSettingMapper.selectByPrimaryKey(query) == null) {
                throw new CommonException("error.userSetting.notFound");
            }
            int update = userSettingMapper.updateByPrimaryKey(query);
            if (update != 1) {
                throw new CommonException("error.userSetting.update");
            }
            userSettingMapper.updateOtherBoardNoDefault(boardId, projectId, userId);
        }
    }

    @Override
    public  BoardDTO createBoard(Long organizationId, Long projectId, String boardName) {
        BoardDTO boardDTO = new BoardDTO();
        boardDTO.setProjectId(projectId);
        boardDTO.setOrganizationId(organizationId);
        boardDTO.setColumnConstraint(CONTRAINT_NONE);
        boardDTO.setDayInColumn(false);
        boardDTO.setEstimationStatistic(STORY_POINTS);
        boardDTO.setName(boardName);
        boardDTO.setSwimlaneBasedCode(PARENT_CHILD);
        if (boardMapper.insertSelective(boardDTO) != 1) {
            throw new CommonException("error.board.insert");
        }
        return boardMapper.selectByPrimaryKey(boardDTO.getBoardId());
    }

    @Override
    public Page<QuickFilterVO> pagedQueryQuickFilters(PageRequest pageRequest,
                                                      Long projectId,
                                                      Long boardId,
                                                      QuickFilterSearchVO quickFilterSearchVO) {
        return quickFilterService.listByProjectId(projectId, quickFilterSearchVO, pageRequest);
    }

    @Override
    public void updateBoardQuickFilterRel(Long projectId,
                                          Long boardId,
                                          List<Long> quickFilterIds) {
        Long organizationId = ConvertUtil.getOrganizationId(projectId);
        BoardQuickFilterRelDTO example = new BoardQuickFilterRelDTO();
        example.setProjectId(projectId);
        example.setOrganizationId(organizationId);
        example.setBoardId(boardId);
        boardQuickFilterRelMapper.delete(example);
        if (!ObjectUtils.isEmpty(quickFilterIds)) {
            quickFilterIds.forEach(id -> {
                BoardQuickFilterRelDTO dto = new BoardQuickFilterRelDTO();
                dto.setProjectId(projectId);
                dto.setOrganizationId(organizationId);
                dto.setBoardId(boardId);
                dto.setQuickFilterId(id);
                if (boardQuickFilterRelMapper.select(dto).isEmpty()) {
                    if (boardQuickFilterRelMapper.insert(dto) != 1) {
                        throw new CommonException("error.board.quickFilter.insert");
                    }
                }
            });
        }
    }

    @Override
    public List<BoardQuickFilterRelVO> listQuickFiltersByBoardId(Long projectId, Long boardId) {
        Long organizationId = ConvertUtil.getOrganizationId(projectId);
        BoardQuickFilterRelDTO example = new BoardQuickFilterRelDTO();
        example.setProjectId(projectId);
        example.setOrganizationId(organizationId);
        example.setBoardId(boardId);
        List<BoardQuickFilterRelDTO> list = boardQuickFilterRelMapper.select(example);
        if (list.isEmpty()) {
            return Collections.emptyList();
        }
        return modelMapper.map(list, new TypeToken<List<BoardQuickFilterRelVO>>() {
        }.getType());
    }

    @Override
    public void initBoard(Long projectId, String boardName, List<StatusPayload> statusPayloads) {
        BoardDTO boardResult = createBoard(0L, projectId, boardName);
        boardColumnService.initBoardColumns(projectId, boardResult.getBoardId(), statusPayloads);
    }

    @Override
    public IssueMoveVO move(Long projectId, Long issueId, Long transformId, IssueMoveVO issueMoveVO, Boolean isDemo) {
        //执行状态机转换
        IssueDTO preIssueDTO = issueMapper.selectByPrimaryKey(issueId);
        if (preIssueDTO == null) {
            throw new CommonException("error.issue.notFound");
        }
        Long preStatusId = preIssueDTO.getStatusId();
        Long nowStatusId = issueMoveVO.getStatusId();
        InputDTO inputDTO = new InputDTO(issueId, UPDATE_STATUS_MOVE, JSON.toJSONString(handleIssueMoveRank(projectId, issueMoveVO)));
        Set<Long> influenceIssueIds = new HashSet<>();
        IssueVO issueVO = issueService.doStateMachineCustomFlowAndRuleNotice(projectId, issueId, SchemeApplyType.AGILE, influenceIssueIds, isDemo, transformId, inputDTO);
        IssueDTO issueDTO = issueMapper.selectByPrimaryKey(issueId);
        if (backlogExpandService != null) {
            backlogExpandService.changeDetection(issueId, projectId, ConvertUtil.getOrganizationId(projectId));
        }
        IssueMoveVO result = modelMapper.map(issueDTO, IssueMoveVO.class);
        if (!preStatusId.equals(nowStatusId)) {
            sendMsgUtil.sendMsgByIssueMoveComplete(projectId, issueMoveVO, issueDTO, DetailsHelper.getUserDetails().getUserId());
        }
        if (!ObjectUtils.isEmpty(issueVO)) {
            result.setErrorMsg(issueVO.getErrorMsg());
        }
        return result;
    }


    protected JSONObject handleIssueMoveRank(Long projectId, IssueMoveVO issueMoveVO) {
        JSONObject jsonObject = new JSONObject();
        if (Boolean.TRUE.equals(issueMoveVO.getRankFlag())) {
            String rank;
            if (Boolean.TRUE.equals(issueMoveVO.getBefore())) {
                rank = getBeforeRank(projectId, issueMoveVO);
            } else {
                rank = getAfterRank(projectId, issueMoveVO);
            }
            jsonObject.put(RANK, rank);
            jsonObject.put(PROJECT_ID, projectId);
            return jsonObject;
        } else {
            return null;
        }
    }

    private String getBeforeRank(Long projectId, IssueMoveVO issueMoveVO) {
        if (issueMoveVO.getOutsetIssueId() == null || Objects.equals(issueMoveVO.getOutsetIssueId(), 0L)) {
            String minRank = sprintMapper.queryMinRank(projectId, issueMoveVO.getSprintId());
            if (minRank == null) {
                return RankUtil.mid();
            } else {
                return RankUtil.genPre(minRank);
            }
        } else {
            String rightRank = issueMapper.queryRank(projectId, issueMoveVO.getOutsetIssueId());
            if (rightRank == null) {
                //处理子任务没有rank的旧数据
                rightRank = handleSubIssueNotRank(projectId, issueMoveVO.getOutsetIssueId(), issueMoveVO.getSprintId());
            }
            String leftRank = issueMapper.queryLeftRank(projectId, issueMoveVO.getSprintId(), rightRank);
            if (leftRank == null) {
                return RankUtil.genPre(rightRank);
            } else {
                return RankUtil.between(leftRank, rightRank);
            }
        }
    }

    private String getAfterRank(Long projectId, IssueMoveVO issueMoveVO){
        String leftRank = issueMapper.queryRank(projectId, issueMoveVO.getOutsetIssueId());
        if (leftRank == null) {
            leftRank = handleSubIssueNotRank(projectId, issueMoveVO.getOutsetIssueId(), issueMoveVO.getSprintId());
        }
        String rightRank = issueMapper.queryRightRank(projectId, issueMoveVO.getSprintId(), leftRank);
        if (rightRank == null) {
            return RankUtil.genNext(leftRank);
        } else {
            return RankUtil.between(leftRank, rightRank);
        }
    }

    private String handleSubIssueNotRank(Long projectId, Long outsetIssueId, Long sprintId) {
        IssueDTO issueDTO = issueMapper.selectByPrimaryKey(outsetIssueId);
        issueDTO.setRank(sprintMapper.queryMaxRank(projectId, sprintId));
        issueMapper.updateByPrimaryKeySelective(issueDTO);
        return issueDTO.getRank();
    }

    @Override
    public List<BoardVO> queryByProjectId(Long projectId) {
        Long userId = DetailsHelper.getUserDetails().getUserId();
        return modelMapper.map(boardMapper.queryByProjectIdWithUser(userId, projectId), new TypeToken<List<BoardVO>>() {
        }.getType());
    }

    @Override
    public UserSettingVO queryUserSettingBoard(Long projectId, Long boardId) {
        UserSettingDTO userSettingDTO = queryUserSettingBoardByBoardId(projectId, boardId, DetailsHelper.getUserDetails().getUserId());
        if (userSettingDTO == null) {
            UserSettingDTO userSetting = new UserSettingDTO();
            userSetting.setProjectId(projectId);
            userSetting.setBoardId(boardId);
            userSetting.setTypeCode(BOARD);
            userSetting.setUserId(DetailsHelper.getUserDetails().getUserId());
            userSetting.setSwimlaneBasedCode("swimlane_none");
            userSetting.setDefaultBoard(false);
            int insert = userSettingMapper.insert(userSetting);
            if (insert != 1) {
                throw new CommonException("error.userSetting.create");
            }
            return modelMapper.map(userSettingMapper.selectByPrimaryKey(userSetting.getSettingId()), UserSettingVO.class);
        } else {
            return modelMapper.map(userSettingDTO, UserSettingVO.class);
        }
    }

    @Override
    public UserSettingVO updateUserSettingBoard(Long projectId, Long boardId, String swimlaneBasedCode) {
        CustomUserDetails customUserDetails = DetailsHelper.getUserDetails();
        Long userId = customUserDetails.getUserId();
        UserSettingDTO userSettingDTO = queryUserSettingBoardByBoardId(projectId, boardId, userId);
        if (userSettingDTO == null) {
            userSettingDTO = new UserSettingDTO();
            userSettingDTO.setDefaultBoard(false);
            userSettingDTO.setTypeCode(BOARD);
            userSettingDTO.setProjectId(projectId);
            userSettingDTO.setBoardId(boardId);
            userSettingDTO.setUserId(userId);
            userSettingDTO.setSwimlaneBasedCode(swimlaneBasedCode);
            userSettingDTO = userSettingService.create(userSettingDTO);
        } else {
            userSettingDTO.setSwimlaneBasedCode(swimlaneBasedCode);
            userSettingDTO = userSettingService.update(userSettingDTO);
        }
        return modelMapper.map(userSettingDTO, UserSettingVO.class);
    }

    private UserSettingDTO queryUserSettingBoardByBoardId(Long projectId, Long boardId, Long userId) {
        UserSettingDTO userSettingDTO = new UserSettingDTO();
        userSettingDTO.setProjectId(projectId);
        userSettingDTO.setBoardId(boardId);
        userSettingDTO.setTypeCode(BOARD);
        userSettingDTO.setUserId(userId);
        return userSettingMapper.selectOne(userSettingDTO);
    }

    @Override
    public Boolean checkName(Long organizationId, Long projectId, String boardName) {
        BoardDTO boardDTO = new BoardDTO();
        boardDTO.setProjectId(projectId);
        boardDTO.setName(boardName);
        boardDTO.setOrganizationId(organizationId);
        List<BoardDTO> boardDTOList = boardMapper.select(boardDTO);
        return boardDTOList != null && !boardDTOList.isEmpty();
    }
}
