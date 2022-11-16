package io.choerodon.agile.app.service.impl;

import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

import org.modelmapper.ModelMapper;
import org.modelmapper.TypeToken;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;

import io.choerodon.agile.api.validator.StoryMapValidator;
import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.api.vo.business.MoveIssueVO;
import io.choerodon.agile.api.vo.business.StoryMapDragVO;
import io.choerodon.agile.app.assembler.BoardAssembler;
import io.choerodon.agile.app.assembler.IssueSearchAssembler;
import io.choerodon.agile.app.assembler.StoryMapAssembler;
import io.choerodon.agile.app.service.*;
import io.choerodon.agile.infra.dto.*;
import io.choerodon.agile.infra.dto.business.StoryMapStoryDTO;
import io.choerodon.agile.infra.mapper.IssueStatusMapper;
import io.choerodon.agile.infra.mapper.SprintMapper;
import io.choerodon.agile.infra.mapper.StoryMapMapper;
import io.choerodon.agile.infra.mapper.StoryMapWidthMapper;
import io.choerodon.agile.infra.utils.PageUtil;
import io.choerodon.core.domain.Page;
import io.choerodon.mybatis.pagehelper.PageHelper;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;

import org.hzero.mybatis.domian.Condition;
import org.hzero.mybatis.util.Sqls;

/**
 * Created by HuangFuqiang@choerodon.io on 2019/5/31.
 * Email: fuqianghuang01@gmail.com
 */
@Service
public class StoryMapServiceImpl implements StoryMapService {

    @Autowired
    private StoryMapMapper storyMapMapper;

    @Autowired
    private IssueAccessDataService issueAccessDataService;

    @Autowired
    private StoryMapValidator storyMapValidator;

    @Autowired
    private VersionIssueRelService versionIssueRelService;

    @Autowired
    private StoryMapWidthMapper storyMapWidthMapper;
    @Autowired
    private StoryMapAssembler storyMapAssembler;
    @Autowired
    private ModelMapper modelMapper;
    @Autowired
    private IssueStatusMapper issueStatusMapper;
    @Autowired(required = false)
    private AgilePluginService agilePluginService;
    @Autowired
    private SprintMapper sprintMapper;
    @Autowired
    private IssueSearchAssembler issueSearchAssembler;
    @Autowired
    private UserService userService;
    @Autowired
    private IssueService issueService;
    @Autowired
    private BoardAssembler boardAssembler;

    protected List<StoryMapWidthVO> setStoryMapWidth(Long projectId) {
        List<StoryMapWidthDTO> storyMapWidthDTOList = storyMapWidthMapper.selectByProjectId(projectId);
        if (storyMapWidthDTOList != null && !storyMapWidthDTOList.isEmpty()) {
            return modelMapper.map(storyMapWidthDTOList, new TypeToken<List<StoryMapWidthVO>>(){}.getType());
        } else {
            return new ArrayList<>();
        }
    }

    @Override
    public StoryMapVO queryStoryMap(Long projectId, Long organizationId, SearchVO searchVO) {
        Boolean condition = issueService.handleSearchUser(searchVO, projectId);
        String filterSql = null;
        if(condition){
            //处理自定义搜索
            if (searchVO.getQuickFilterIds() != null && !searchVO.getQuickFilterIds().isEmpty()) {
                filterSql = issueService.getQuickFilter(searchVO.getQuickFilterIds());
            }
            //处理未匹配的筛选
            boardAssembler.handleOtherArgs(searchVO);
        }
        StoryMapVO storyMap = new StoryMapVO();
        List<Long> epicIds = new ArrayList<>();
        // get project completed status
        getStatusIdByIsCompleted(projectId, searchVO);
        // get project epic
        List<Long> projectEpicIds = storyMapMapper.selectEpicIdsByProject(projectId, searchVO);
        if (projectEpicIds != null && !projectEpicIds.isEmpty()) {
            epicIds.addAll(projectEpicIds);
        }
        if (agilePluginService != null) {
            storyMap = agilePluginService.handlerBusinessQueryStoryMap(projectId, epicIds, searchVO);
        }
        else {
            List<StoryMapStoryDTO> storyMapStoryDTOS = new ArrayList<>();
            if (epicIds.isEmpty()) {
                storyMap.setEpics(new ArrayList<>());
            } else {
                List<EpicWithInfoDTO> epicWithInfoDTOList = storyMapMapper.selectEpicList(projectId, epicIds, searchVO.getAdvancedSearchArgs());
                storyMap.setEpics(epicWithInfoDTOList);
                storyMapStoryDTOS.addAll(storyMapMapper.selectStoryList(new HashSet<>(Arrays.asList(projectId)), epicIds, searchVO,filterSql,searchVO.getAssigneeFilterIds()));
            }
            storyMap.setStoryList(!epicIds.isEmpty() ? storyMapStoryDTOS : new ArrayList<>());
        }
        storyMap.setStoryMapWidth(setStoryMapWidth(projectId));
        return storyMap;
    }

    protected void getStatusIdByIsCompleted(Long projectId, SearchVO searchVO) {
        Boolean completedFlag = (Boolean) Optional.ofNullable(searchVO.getAdvancedSearchArgs())
                .map(map -> map.get("isCompleted")).orElse(null);
        if (Objects.nonNull(completedFlag)){
            List<IssueStatusDTO> statusIdList = issueStatusMapper.selectByCondition(Condition.builder(IssueStatusDTO.class)
                    .andWhere(Sqls.custom().andEqualTo(IssueStatusDTO.FIELD_PROJECT_ID, projectId)
                            .andEqualTo(IssueStatusDTO.FILED_IS_COMPLETED, completedFlag)).build());
            searchVO.getAdvancedSearchArgs().put("statusIdList",
                    statusIdList.stream().map(IssueStatusDTO::getStatusId).collect(Collectors.toList()));
        }
    }

    @Override
    public Page<StoryMapStoryVO> queryStoryMapDemand(Long projectId, SearchVO searchVO, PageRequest pageRequest) {
        boardAssembler.handleAdvanceSearch(searchVO);
        Page<StoryMapStoryDTO> page = PageHelper.doPage(pageRequest, () -> storyMapMapper.selectDemandStoryList(projectId, searchVO));
        List<StoryMapStoryVO> storyMapStoryVOList = storyMapAssembler.storyMapStoryDTOToVO(projectId, page.getContent());
        return PageUtil.buildPageInfoWithPageInfoList(page, storyMapStoryVOList);
    }

    protected void dragToEpic(Long projectId, Long epicId, StoryMapDragVO storyMapDragVO) {
        storyMapValidator.checkEpicExist(epicId);
        List<Long> issueIds = storyMapDragVO.getEpicIssueIds();
        if (issueIds != null && !issueIds.isEmpty()) {
            issueAccessDataService.batchIssueToEpic(projectId, epicId, issueIds);
        }
    }

    protected void dragToVersion(Long projectId, Long versionId, StoryMapDragVO storyMapDragVO) {
        List<VersionIssueRelVO> versionIssueRelVOList = storyMapDragVO.getVersionIssueRelVOList();
        if (versionIssueRelVOList != null && !versionIssueRelVOList.isEmpty()) {
            for (VersionIssueRelVO versionIssueRelVO : versionIssueRelVOList) {
                VersionIssueRelDTO versionIssueRelDTO = new VersionIssueRelDTO();
                versionIssueRelDTO.setIssueId(versionIssueRelVO.getIssueId());
                versionIssueRelDTO.setVersionId(versionIssueRelVO.getVersionId());
                versionIssueRelDTO.setRelationType(ProductVersionService.VERSION_RELATION_TYPE_FIX);
                versionIssueRelDTO.setProjectId(projectId);
                versionIssueRelService.delete(versionIssueRelDTO);
            }
        }
        storyMapValidator.checkVersionExist(versionId);
        List<Long> issueIds = storyMapDragVO.getVersionIssueIds();
        if (issueIds == null || issueIds.isEmpty()) {
            return;
        }
        if (!Objects.equals(versionId, 0L)) {
            VersionIssueRelDTO versionIssueRelDTO = new VersionIssueRelDTO();
            versionIssueRelDTO.createBatchIssueToVersionDTO(projectId, versionId, issueIds);
            issueAccessDataService.batchIssueToVersion(versionIssueRelDTO);
        }
    }

    @Override
    public void storyMapMove(Long projectId, StoryMapDragVO storyMapDragVO) {
        Long epicId = storyMapDragVO.getEpicId();
        Long versionId = storyMapDragVO.getVersionId();
        Long sprintId = storyMapDragVO.getSprintId();
        if (epicId != null) {
            dragToEpic(projectId, epicId, storyMapDragVO);
        }
        if (versionId != null) {
            dragToVersion(projectId, versionId, storyMapDragVO);
        }

        if (agilePluginService != null) {
            agilePluginService.handlerStoryMapMoveFeature(projectId,storyMapDragVO);
        }

        if (sprintId != null) {
            dragToSprint(projectId, sprintId, storyMapDragVO);
        }
    }

    private void dragToSprint(Long projectId, Long sprintId, StoryMapDragVO storyMapDragVO) {
        List<Long> sprintIssueIds = storyMapDragVO.getSprintIssueIds();
        if (!CollectionUtils.isEmpty(sprintIssueIds)) {
            MoveIssueVO moveIssueVO = new MoveIssueVO();
            moveIssueVO.setIssueIds(sprintIssueIds);
            moveIssueVO.setRankIndex(false);
            Long outIssueId = sprintMapper.queryOutIssueId(projectId, sprintId);
            moveIssueVO.setOutsetIssueId(ObjectUtils.isEmpty(outIssueId) ? 0L : outIssueId);
            moveIssueVO.setBefore(ObjectUtils.isEmpty(outIssueId));
            moveIssueVO.setRankIndex(false);
            issueService.batchIssueToSprint(projectId, sprintId, moveIssueVO);
        }
    }

    @Override
    public List<SprintSearchVO> storyMapSprintInfo(Long projectId, List<Long> sprintIds) {
        // 查询项目的所有冲刺
        List<SprintDTO> sprintDTOS = sprintMapper.selectByCondition(Condition.builder(SprintDTO.class).andWhere(Sqls.custom().andEqualTo("projectId", projectId)).build());
        if (CollectionUtils.isEmpty(sprintDTOS)) {
            return new ArrayList<>();
        }
        Collections.sort(sprintDTOS, ((o1, o2) -> o2.getSprintId().compareTo(o1.getSprintId())));
        if (CollectionUtils.isEmpty(sprintIds)) {
            // 查询冲刺经办人的汇总信息
            sprintIds = sprintDTOS.stream().map(SprintDTO::getSprintId).limit(5L).collect(Collectors.toList());
        } else {
            List<Long> finalSprintIds = sprintIds;
            sprintDTOS = sprintDTOS.stream().filter(sprintDTO -> finalSprintIds.contains(sprintDTO.getSprintId())).collect(Collectors.toList());
        }
        List<AssigneeIssueDTO> assigneeIssueDTOS = sprintMapper.queryAssigneeIssueBySprintIds(projectId, sprintIds);
        Map<Long, List<AssigneeIssueDTO>> assigneeCountMap = new HashMap<>();
        Map<Long, UserMessageDTO> usersMap = new HashMap<>();
        if (!CollectionUtils.isEmpty(assigneeIssueDTOS)) {
            Set<Long> assigneeIds = assigneeIssueDTOS.stream().map(AssigneeIssueDTO::getAssigneeId).collect(Collectors.toSet());
            usersMap.putAll(userService.queryUsersMap(new ArrayList<>(assigneeIds), true));
            assigneeCountMap.putAll(assigneeIssueDTOS.stream().collect(Collectors.groupingBy(AssigneeIssueDTO::getSprintId)));
        }
        // 查询冲刺故事点的完成情况
        List<SprintSearchVO> issueProgressVOS = sprintMapper.queryStoryPointProgress(projectId, sprintIds);
        Map<Long, SprintSearchVO> issueProgressVOMap = issueProgressVOS.stream().collect(Collectors.toMap(SprintSearchVO::getSprintId, Function.identity()));
        List<SprintSearchVO> list = new ArrayList<>();
        for (SprintDTO sprintDTO : sprintDTOS) {
            SprintSearchVO sprint = modelMapper.map(sprintDTO, SprintSearchVO.class);
            sprint.setAssigneeIssues(issueSearchAssembler.dtoListToAssigneeIssueVO(assigneeCountMap.get(sprint.getSprintId()), usersMap));
            SprintSearchVO issueProgressVO = issueProgressVOMap.get(sprintDTO.getSprintId());
            if (!ObjectUtils.isEmpty(issueProgressVO)) {
                sprint.setTodoStoryPoint(issueProgressVO.getTodoStoryPoint());
                sprint.setDoingStoryPoint(issueProgressVO.getDoingStoryPoint());
                sprint.setDoneStoryPoint(issueProgressVO.getDoneStoryPoint());
            }
            list.add(sprint);
        }
        if (agilePluginService != null) {
            agilePluginService.handlerSprintPlanInfo(projectId,list);
        }
        return list;
    }

    @Override
    public StoryMapVO pageStoryMap(Long projectId, Long organizationId, SearchVO searchVO, Integer page, Integer size) {
        Boolean condition = issueService.handleSearchUser(searchVO, projectId);
        String filterSql = null;
        if(condition){
            //处理自定义搜索
            if (searchVO.getQuickFilterIds() != null && !searchVO.getQuickFilterIds().isEmpty()) {
                filterSql = issueService.getQuickFilter(searchVO.getQuickFilterIds());
            }
            //处理未匹配的筛选
            boardAssembler.handleOtherArgs(searchVO);
        }
        StoryMapVO storyMap = new StoryMapVO();
        List<Long> epicIds = new ArrayList<>();
        // get project completed status
        getStatusIdByIsCompleted(projectId, searchVO);
        if (agilePluginService != null) {
            // get project epic
            List<Long> projectEpicIds = storyMapMapper.selectEpicIdsByProject(projectId, searchVO);
            if (projectEpicIds != null && !projectEpicIds.isEmpty()) {
                epicIds.addAll(projectEpicIds);
            }
            storyMap = agilePluginService.handlerBusinessPageStoryMap(projectId, epicIds, searchVO, page, size);
        }
        else {
            Page<Long> pageEpicIds = PageHelper.doPage(page, size, () -> storyMapMapper.selectEpicIdsByProject(projectId, searchVO));
            List<Long> content = pageEpicIds.getContent();
            if (CollectionUtils.isEmpty(content)) {
                storyMap.setEpics(new ArrayList<>());
            } else {
                List<EpicWithInfoDTO> epicWithInfoDTOList = storyMapMapper.selectEpicList(projectId, content, searchVO.getAdvancedSearchArgs());
                storyMap.setEpics(epicWithInfoDTOList);
            }
            List<StoryMapStoryDTO> storyMapStoryDTOS = storyMapMapper.selectStoryList(new HashSet<>(Arrays.asList(projectId)), content, searchVO,filterSql,searchVO.getAssigneeFilterIds());
            storyMap.setStoryList(!content.isEmpty() ? storyMapStoryDTOS : new ArrayList<>());
            storyMap.setTotalPage(pageEpicIds.getTotalPages());
        }
        storyMap.setPage(page);
        storyMap.setSize(size);
        storyMap.setStoryMapWidth(setStoryMapWidth(projectId));
        return storyMap;
    }
}
