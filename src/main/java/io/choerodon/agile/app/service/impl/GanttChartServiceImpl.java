package io.choerodon.agile.app.service.impl;

import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.api.vo.business.TagVO;
import io.choerodon.agile.app.assembler.BoardAssembler;
import io.choerodon.agile.app.service.*;
import io.choerodon.agile.infra.dto.*;
import io.choerodon.agile.infra.dto.business.IssueDTO;
import io.choerodon.agile.infra.enums.FieldCode;
import io.choerodon.agile.infra.enums.GanttDimension;
import io.choerodon.agile.infra.enums.IssueTypeCode;
import io.choerodon.agile.infra.feign.BaseFeignClient;
import io.choerodon.agile.infra.mapper.*;
import io.choerodon.agile.infra.utils.ConvertUtil;
import io.choerodon.agile.infra.utils.PageUtil;
import io.choerodon.agile.infra.utils.*;
import io.choerodon.core.domain.Page;
import io.choerodon.core.exception.CommonException;
import io.choerodon.core.utils.PageUtils;
import io.choerodon.core.utils.PageableHelper;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;
import io.choerodon.mybatis.pagehelper.domain.Sort;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.ObjectUtils;

import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * @author superlee
 * @since 2020-11-24
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class GanttChartServiceImpl implements GanttChartService {

    private static final String ISSUE_ID = "issueId";
    private static final String ORDER_STR = "orderStr";
    private static final String ISSUE_TYPE_ID = "issueTypeId";
    private static final String TYPE_CODES = "typeCodes";
    private static final String SPRINT = "sprint";
    private static final String CREATE_USER = "createUser";
    private static final String UPDATE_USER = "updateUser";
    private static final String MAIN_RESPONSIBLE_USER = "mainResponsibleUser";
    private static final String TAGS = "tags";
    private static final String PARTICIPANTS = "participants";
    private static final String ERROR_SPRINT_EMPTY = "error.otherArgs.sprint.empty";
    private static final String ERROR_GANTT_DIMENSION_NOT_SUPPORT = "error.gantt.dimension.not.support";
    private static final String ERROR_GANTT_MOVE_NULL_DATA = "error.gantt.move.null.data";
    private static final List<String> SPECIAL_HANDLER_SYSTEM_FIELD =
            Arrays.asList(
                    FieldCode.LABEL,
                    FieldCode.COMPONENT,
                    FieldCode.FIX_VERSION,
                    FieldCode.INFLUENCE_VERSION,
                    FieldCode.ASSIGNEE,
                    FieldCode.REPORTER,
                    CREATE_USER,
                    UPDATE_USER,
                    MAIN_RESPONSIBLE_USER,
                    FieldCode.SPRINT,
                    TAGS,
                    PARTICIPANTS);

    @Autowired
    private IssueService issueService;
    @Autowired
    private BoardAssembler boardAssembler;
    @Autowired
    private IssueMapper issueMapper;
    @Autowired
    private IssueTypeMapper issueTypeMapper;
    @Autowired
    private UserService userService;
    @Autowired
    private StatusService statusService;
    @Autowired
    private IssueStatusMapper issueStatusMapper;
    @Autowired
    private IssueSprintRelMapper issueSprintRelMapper;
    @Autowired(required = false)
    private AgilePluginService agilePluginService;
    @Autowired
    private BaseFeignClient baseFeignClient;
    @Autowired
    private ObjectSchemeFieldMapper objectSchemeFieldMapper;
    @Autowired
    private PageFieldService pageFieldService;
    @Autowired
    private IssueLabelMapper issueLabelMapper;
    @Autowired
    private IssueComponentMapper issueComponentMapper;
    @Autowired
    private VersionIssueRelMapper versionIssueRelMapper;
    @Autowired
    private LookupValueService lookupValueService;
    @Autowired
    private GanttIssueRankMapper ganttIssueRankMapper;
    @Autowired
    private GanttDimensionRankMapper ganttDimensionRankMapper;
    @Autowired
    private PriorityService priorityService;

    @Override
    public Page<GanttChartVO> pagedQuery(Long projectId,
                                         SearchVO searchVO,
                                         PageRequest pageRequest) {
        if (isSprintEmpty(searchVO)) {
            throw new CommonException(ERROR_SPRINT_EMPTY);
        }
        Long organizationId = ConvertUtil.getOrganizationId(projectId);
        Map<Long, ProjectVO> projectMap = queryProjectMap(projectId);
        return listByProjectIdAndSearch(projectMap, searchVO, pageRequest, organizationId, true);
    }

    private Map<Long, ProjectVO> queryProjectMap(Long projectId) {
        Map<Long, ProjectVO> projectMap = new HashMap<>();
        ProjectVO projectVO = ConvertUtil.queryProject(projectId);
        AssertUtilsForCommonException.notNull(projectVO, "error.gantt.project.null");
        projectMap.put(projectVO.getId(), projectVO);
        return projectMap;
    }

    private void validateDisplayFields(SearchVO searchVO) {
        List<ObjectSchemeFieldVO> displayFields = searchVO.getDisplayFields();
        if (CollectionUtils.isEmpty(displayFields)) {
            displayFields = new ArrayList<>();
            searchVO.setDisplayFields(displayFields);
        } else {
            displayFields.forEach(field ->
                    AssertUtilsForCommonException.notNull(field.getCode(), "error.gantt.display.field.code.null"));
        }
    }

    private void validateDimension(String dimension) {
        AssertUtilsForCommonException.notNull(dimension, "error.gantt.dimension.null");
        if (!GanttDimension.contains(dimension)) {
            throw new CommonException("error.illegal.gantt.dimension");
        }
        if (GanttDimension.isFeature(dimension)) {
            throw new CommonException(ERROR_GANTT_DIMENSION_NOT_SUPPORT);
        }
    }

    private String getDimensionFromSearchVO(SearchVO searchVO) {
        Map<String, Object> searchArgs = searchVO.getSearchArgs();
        if (ObjectUtils.isEmpty(searchArgs)) {
            throw new CommonException("error.gantt.dimension.null");
        }
        return (String) searchArgs.get("dimension");
    }

    @Override
    public List<GanttChartVO> listByIds(Long projectId,
                                        GanttChartSearchVO ganttChartSearchVO,
                                        String dimension) {
        Set<Long> issueIds = ganttChartSearchVO.getIssueIds();
        if (ObjectUtils.isEmpty(issueIds)) {
            return new ArrayList<>();
        }
        List<ObjectSchemeFieldVO> displayFields = ganttChartSearchVO.getDisplayFields();
        if (CollectionUtils.isEmpty(displayFields)) {
            displayFields = new ArrayList<>();
        }
        validateDimension(dimension);
        Long organizationId = ConvertUtil.getOrganizationId(projectId);
        Set<Long> projectIds = new HashSet<>();
        projectIds.add(projectId);
        List<IssueDTO> issueList = issueMapper.selectWithSubByIssueIds(projectIds, new ArrayList<>(issueIds), null, false, null);
        Map<Long, ProjectVO> projectMap = queryProjectMap(projectId);
        return buildGanttList(projectMap, new ArrayList<>(issueIds), issueList, new HashMap<>(), new HashMap<>(), displayFields, organizationId);
    }

    @Override
    public void move(Long projectId, GanttMoveVO ganttMoveVO) {
        String dimension = ganttMoveVO.getDimension();
        String instanceType = ganttMoveVO.getInstanceType();
        Long instanceId = ganttMoveVO.getInstanceId();
        moveValidator(ganttMoveVO, dimension, instanceType);
        SearchVO searchVO = validateAndProcessSearchVO(projectId, ganttMoveVO.getSearchVO());
        SearchVO searchWithRequiredFilter = copyRequiredFilter(searchVO);
        Long previousId = ganttMoveVO.getPreviousId();
        Long nextId = ganttMoveVO.getNextId();
        Long currentId = ganttMoveVO.getCurrentId();
        Long organizationId = ConvertUtil.getOrganizationId(projectId);
        //查询没有额外筛选条件的当前维度当前基准的排序
        LinkedHashMap<Long, String> issueWithRankMap =
                queryIssueListByInstanceType(projectId, searchWithRequiredFilter, instanceId, instanceType, dimension, currentId);
        validateMoveId(issueWithRankMap, previousId, nextId, currentId);
        if (nextId == null) {
            nextId = queryNextIdByPreviousId(previousId, issueWithRankMap);
        }
        String currentRank;
        if (nextId == null) {
            //该维度没有设置排序，currentId设置为RankUtil.mid()
            currentRank = RankUtil.mid();
        } else {
            String nextRank = issueWithRankMap.get(nextId);
            if (StringUtils.isEmpty(nextRank)) {
                nextRank =
                        initIssueRankIfNull(projectId, dimension, instanceType, instanceId, nextId, organizationId, issueWithRankMap);
            }
            String previousRank =
                    ganttIssueRankMapper.selectMaxPreviousRankOrderByRankAsc(organizationId, projectId, instanceId, instanceType, dimension, nextRank);
            if (StringUtils.isEmpty(previousRank)) {
                currentRank = RankUtil.genPre(nextRank);
            } else {
                currentRank = RankUtil.between(previousRank, nextRank);
            }
        }
        updateGanttIssueRank(currentId, organizationId, projectId, instanceId, instanceType, dimension, currentRank);
    }

    private void validateMoveId(LinkedHashMap<Long, String> rankMap,
                                Long previousId,
                                Long nextId,
                                Long currentId) {
        if (!ObjectUtils.isEmpty(previousId) && !rankMap.containsKey(previousId)) {
            throw new CommonException("error.gantt.move.illegal.previousId");
        }
        if (!ObjectUtils.isEmpty(nextId) && !rankMap.containsKey(nextId)) {
            throw new CommonException("error.gantt.move.illegal.nextId");
        }
        if (!rankMap.containsKey(currentId)) {
            throw new CommonException("error.gantt.move.illegal.currentId");
        }
    }

    @Override
    public void moveDimension(Long projectId,
                              GanttDimensionMoveVO ganttDimensionMoveVO) {
        String dimension = ganttDimensionMoveVO.getDimension();
        moveDimensionValidator(ganttDimensionMoveVO, dimension);
        SearchVO searchVO = validateAndProcessSearchVO(projectId, ganttDimensionMoveVO.getSearchVO());
        SearchVO searchWithRequiredFilter = copyRequiredFilter(searchVO);
        Long previousId = ganttDimensionMoveVO.getPreviousId();
        Long nextId = ganttDimensionMoveVO.getNextId();
        Long currentId = ganttDimensionMoveVO.getCurrentId();
        Long organizationId = ConvertUtil.getOrganizationId(projectId);
        LinkedHashMap<Long, String> instanceRankMap = queryInstanceRankMap(projectId, searchWithRequiredFilter, dimension);
        validateMoveId(instanceRankMap, previousId, nextId, currentId);
        if (nextId == null) {
            nextId = queryNextIdByPreviousId(previousId, instanceRankMap);
        }
        String currentRank;
        if (nextId == null) {
            //该维度没有设置排序，currentId设置为RankUtil.mid()
            currentRank = RankUtil.mid();
        } else {
            String nextRank = instanceRankMap.get(nextId);
            if (StringUtils.isEmpty(nextRank)) {
                nextRank =
                        initDimensionRankIfNull(projectId, dimension, nextId, organizationId, instanceRankMap);
            }
            String previousRank =
                    ganttDimensionRankMapper.selectMaxPreviousRankOrderByRankAsc(organizationId, projectId, dimension, nextRank);
            if (StringUtils.isEmpty(previousRank)) {
                currentRank = RankUtil.genPre(nextRank);
            } else {
                currentRank = RankUtil.between(previousRank, nextRank);
            }
        }
        updateGanttDimensionRank(currentId, organizationId, projectId, dimension, currentRank);
    }

    private void updateGanttDimensionRank(Long instanceId,
                                          Long organizationId,
                                          Long projectId,
                                          String dimension,
                                          String rank) {
        GanttDimensionRankDTO example = new GanttDimensionRankDTO();
        example.setInstanceId(instanceId);
        example.setInstanceType(dimension);
        example.setDimension(dimension);
        example.setProjectId(projectId);
        example.setOrganizationId(organizationId);
        List<GanttDimensionRankDTO> ganttDimensionRankList = ganttDimensionRankMapper.select(example);
        if (ganttDimensionRankList.isEmpty()) {
            example.setRank(rank);
            if (ganttDimensionRankMapper.insert(example) != 1) {
                throw new CommonException("error.gantt.dimension.update.rank");
            }
        } else {
            GanttDimensionRankDTO dto = ganttDimensionRankList.get(0);
            dto.setRank(rank);
            if (ganttDimensionRankMapper.updateByPrimaryKey(dto) != 1) {
                throw new CommonException("error.gantt.dimension.update.rank");
            }
        }

    }

    private void moveDimensionValidator(GanttDimensionMoveVO ganttDimensionMoveVO, String dimension) {
        if (!GanttDimension.isSprint(dimension)
                && !GanttDimension.isAssignee(dimension)) {
            throw new CommonException(ERROR_GANTT_DIMENSION_NOT_SUPPORT);
        }
        Long previewId = ganttDimensionMoveVO.getPreviousId();
        Long nextId = ganttDimensionMoveVO.getNextId();
        if (ObjectUtils.isEmpty(previewId) && ObjectUtils.isEmpty(nextId)) {
            throw new CommonException("error.gantt.move.target.id.null");
        }
    }

    @Override
    public GanttDimensionListVO ganttDimensionList(Long projectId, SearchVO searchVO) {
        if (isSprintEmpty(searchVO)) {
            throw new CommonException(ERROR_SPRINT_EMPTY);
        }
        String dimension = getDimensionFromSearchVO(searchVO);
        if (!GanttDimension.isSprint(dimension)
                && !GanttDimension.isAssignee(dimension)) {
            throw new CommonException(ERROR_GANTT_DIMENSION_NOT_SUPPORT);
        }
        GanttDimensionListVO result = new GanttDimensionListVO();
        result.setIds(new ArrayList<>());
        LinkedHashMap<Long, String> instanceRankMap = queryInstanceRankMap(projectId, searchVO, dimension);
        List<Long> ids = new ArrayList<>();
        instanceRankMap.forEach((k, v) -> ids.add(k));
        result.setIds(ids);
        return result;
    }

    private LinkedHashMap<Long, String> queryInstanceRankMap(Long projectId,
                                                             SearchVO searchVO,
                                                             String dimension) {
        PageRequest pageRequest = new PageRequest(1, 0);
        buildIssueType(searchVO);
        Boolean condition = issueService.handleSearchUser(searchVO, projectId);
        if (!Boolean.TRUE.equals(condition)) {
            return new LinkedHashMap<>();
        }
        String filterSql = getFilterSql(searchVO);
        boardAssembler.handleOtherArgs(searchVO);
        boolean isTreeView =
                Boolean.TRUE.equals(
                        Optional.ofNullable(searchVO.getSearchArgs())
                                .map(x -> x.get("tree"))
                                .orElse(true));
        Page<Long> page = issueService.pagedQueryByTreeView(pageRequest, new HashSet<>(Arrays.asList(projectId)), searchVO, filterSql, null, isTreeView);
        List<Long> issueIds = page.getContent();
        if (issueIds.isEmpty()) {
            return new LinkedHashMap<>();
        }
        return queryOrderedInstanceIdsWithRank(projectId, dimension, issueIds);
    }

    private LinkedHashMap<Long, String> queryOrderedInstanceIdsWithRank(Long projectId,
                                                                        String dimension,
                                                                        List<Long> issueIds) {
        Set<Long> instanceIds = new HashSet<>();
        GanttDimension ganttDimension = GanttDimension.valueOf(dimension.toUpperCase());
        switch (ganttDimension) {
            case ASSIGNEE:
                instanceIds.addAll(issueMapper.selectAssigneeIdByIssueIds(projectId, issueIds));
                break;
            case SPRINT:
                Map<Long, IssueSprintDTO> issueSprintMap = queryIssueSprint(new HashSet<>(Arrays.asList(projectId)), issueIds);
                issueSprintMap.forEach((issueId, sprint) -> instanceIds.add(sprint.getSprintId()));
                break;
            default:
                break;
        }
        if (instanceIds.isEmpty()) {
            return new LinkedHashMap<>();
        }
        List<GanttDimensionRankDTO> ganttDimensionRankList =
                ganttDimensionRankMapper.orderByInstanceId(instanceIds, projectId, dimension);
        LinkedHashMap<Long, String> rankMap = new LinkedHashMap<>();
        ganttDimensionRankList.forEach(x -> rankMap.put(x.getInstanceId(), x.getRank()));
        List<Long> noRankList = new ArrayList<>();
        instanceIds.forEach(id -> {
            if (!rankMap.containsKey(id)) {
                noRankList.add(id);
            }
        });
        noRankList.sort(Long::compareTo);
        LinkedHashMap<Long, String> resultMap = new LinkedHashMap<>();
        noRankList.forEach(id -> resultMap.put(id, null));
        rankMap.forEach((k, v) -> resultMap.put(k, v));
        return resultMap;
    }


    private String initIssueRankIfNull(Long projectId,
                                       String dimension,
                                       String instanceType,
                                       Long instanceId,
                                       Long nextId,
                                       Long organizationId,
                                       LinkedHashMap<Long, String> issueWithRankMap) {
        List<Long> nullRankIssueIds = new ArrayList<>();
        //rank 升序
        List<String> rankList = new ArrayList<>();
        processNullRankList(nullRankIssueIds, rankList, issueWithRankMap, nextId);
        List<GanttIssueRankDTO> insertList = new ArrayList<>();
        int nullRankSize = nullRankIssueIds.size();
        for (int i = 0; i < nullRankSize; i++) {
            Long issueId = nullRankIssueIds.get(i);
            String thisRank = rankList.get(i);
            insertList.add(buildGanttIssueRank(organizationId, projectId, instanceId, instanceType, dimension, issueId, thisRank));
        }
        ganttIssueRankMapper.batchInsert(insertList);
        return rankList.get(0);
    }

    private String initDimensionRankIfNull(Long projectId,
                                           String dimension,
                                           Long nextId,
                                           Long organizationId,
                                           LinkedHashMap<Long, String> instanceRankMap) {
        List<Long> nullRankIssueIds = new ArrayList<>();
        //rank 升序
        List<String> rankList = new ArrayList<>();
        processNullRankList(nullRankIssueIds, rankList, instanceRankMap, nextId);
        List<GanttDimensionRankDTO> insertList = new ArrayList<>();
        int nullRankSize = nullRankIssueIds.size();
        for (int i = 0; i < nullRankSize; i++) {
            Long instanceId = nullRankIssueIds.get(i);
            String thisRank = rankList.get(i);
            insertList.add(buildGanttDimensionRank(organizationId, projectId, instanceId, dimension, thisRank));
        }
        ganttDimensionRankMapper.batchInsert(insertList);
        return rankList.get(0);
    }

    private GanttDimensionRankDTO buildGanttDimensionRank(Long organizationId,
                                                          Long projectId,
                                                          Long instanceId,
                                                          String dimension,
                                                          String rank) {
        GanttDimensionRankDTO dto = new GanttDimensionRankDTO();
        dto.setOrganizationId(organizationId);
        dto.setProjectId(projectId);
        dto.setInstanceId(instanceId);
        dto.setInstanceType(dimension);
        dto.setDimension(dimension);
        dto.setRank(rank);
        return dto;
    }

    private void processNullRankList(List<Long> nullRankIds,
                                     List<String> rankList,
                                     LinkedHashMap<Long, String> idWithRankMap,
                                     Long nextId) {
        String minRank = null;
        boolean afterNextId = false;
        for (Map.Entry<Long, String> entry : idWithRankMap.entrySet()) {
            Long id = entry.getKey();
            String thisRank = entry.getValue();
            if (id.equals(nextId)) {
                afterNextId = true;
            }
            if (afterNextId) {
                if (StringUtils.isEmpty(thisRank)) {
                    nullRankIds.add(id);
                } else {
                    minRank = thisRank;
                    break;
                }
            }
        }
        int nullRankSize = nullRankIds.size();
        String[] rankArrays = new String[nullRankSize];
        if (minRank == null) {
            minRank = RankUtil.mid();
        }
        for (int i = 0; i < nullRankSize; i++) {
            String thisRank = RankUtil.genPre(minRank);
            minRank = thisRank;
            rankArrays[nullRankSize - i - 1] = thisRank;
        }
        rankList.addAll(Arrays.asList(rankArrays));
    }

    private Long queryNextIdByPreviousId(Long previousId, LinkedHashMap<Long, String> issueWithRankMap) {
        //previousId不为空
        boolean afterPreviousId = false;
        Long nextId = null;
        for (Map.Entry<Long, String> entry : issueWithRankMap.entrySet()) {
            Long issueId = entry.getKey();
            if (afterPreviousId) {
                nextId = issueId;
                break;
            }
            if (issueId.equals(previousId)) {
                afterPreviousId = true;
            }
        }
        return nextId;
    }

    private SearchVO copyRequiredFilter(SearchVO searchVO) {
        SearchVO searchWithRequiredFilter = new SearchVO();
        Map<String, Object> advancedSearchArgs = new HashMap<>();
        searchWithRequiredFilter.setAdvancedSearchArgs(advancedSearchArgs);
        Map<String, Object> searchArgs = new HashMap<>();
        searchWithRequiredFilter.setSearchArgs(searchArgs);
        Map<String, Object> otherArgs = new HashMap<>();
        searchWithRequiredFilter.setOtherArgs(otherArgs);
        advancedSearchArgs.put(ISSUE_TYPE_ID, searchVO.getAdvancedSearchArgs().get(ISSUE_TYPE_ID));
        otherArgs.put(SPRINT, searchVO.getOtherArgs().get(SPRINT));
        boolean isTreeView =
                Boolean.TRUE.equals(
                        Optional.ofNullable(searchVO.getSearchArgs())
                                .map(x -> x.get("tree"))
                                .orElse(true));
        searchArgs.put("tree", isTreeView);
        return searchWithRequiredFilter;
    }

    private SearchVO validateAndProcessSearchVO(Long projectId,
                                                SearchVO searchVO) {
        buildIssueType(searchVO);
        boolean condition = issueService.handleSearchUser(searchVO, projectId);
        if (!condition) {
            throw new CommonException("error.illegal.gantt.searchVO");
        }
        if (isSprintEmpty(searchVO)) {
            throw new CommonException(ERROR_SPRINT_EMPTY);
        }
        boardAssembler.handleOtherArgs(searchVO);
        return searchVO;
    }

    private LinkedHashMap<Long, String> queryIssueListByInstanceType(Long projectId,
                                                                     SearchVO searchVO,
                                                                     Long instanceId,
                                                                     String instanceType,
                                                                     String dimension,
                                                                     Long currentId) {
        PageRequest pageRequest = new PageRequest(1, 0);
        boolean isTreeView = (boolean) searchVO.getSearchArgs().get("tree");
        addDimensionIfNotExisted(searchVO, dimension);
        Map<String, Object> sortMap = new HashMap<>();
        addGanttDefaultOrder(searchVO, pageRequest);
        boardAssembler.handleOtherArgs(searchVO);
        processSort(pageRequest, sortMap);
        List<Long> issueIds;
        Set<Long> projectIds = new HashSet<>(Arrays.asList(projectId));
        if (GanttDimension.isTask(instanceType) && !Objects.equals(0L, instanceId)) {
            //子任务拖动，查出子任务
            List<IssueDTO> childIssues = issueMapper.queryChildrenIdByParentId(Arrays.asList(instanceId), projectIds, searchVO, null, searchVO.getAssigneeFilterIds(), sortMap);
            issueIds = childIssues.stream().map(IssueDTO::getIssueId).collect(Collectors.toList());
        } else if (GanttDimension.isEpic(instanceType) && !Objects.equals(0L, instanceId)) {
            //在史诗下的拖动
            issueIds = queryUnderEpicIssueIds(projectIds, searchVO, instanceId, instanceType, currentId, isTreeView, sortMap);
        } else if (GanttDimension.isEpic(instanceType) && Objects.equals(0L, instanceId)) {
            //在未分配史诗下的拖动或根目录拖动
            issueIds = queryRootOrNoEpicIssueIds(projectIds, searchVO, isTreeView, sortMap, currentId);
        } else if (GanttDimension.isFeature(instanceType) && Objects.equals(0L, instanceId)) {
            //未分配特性下拖动
            issueIds = queryNoFeatureIssueIds(projectIds, searchVO, isTreeView, sortMap);
        } else {
            addFilterConditionByInstanceType(searchVO, instanceType, instanceId);
            issueIds = issueService.listByTreeView(projectIds, searchVO, null, sortMap, isTreeView);
        }
        AssertUtilsForCommonException.notEmpty(issueIds, ERROR_GANTT_MOVE_NULL_DATA);
        Long organizationId = ConvertUtil.getOrganizationId(projectId);
        Map<Long, String> rankMap = new HashMap<>();
        ganttIssueRankMapper.selectByIssueIdWithRank(organizationId, projectId, instanceId, instanceType, dimension, issueIds)
                .forEach(dto -> {
                    Long issueId = dto.getIssueId();
                    if (issueId == null) {
                        return;
                    }
                    rankMap.put(issueId, dto.getRank());
                });
        LinkedHashMap<Long, String> linkedHashMap = new LinkedHashMap<>();
        issueIds.forEach(issueId -> linkedHashMap.put(issueId, rankMap.get(issueId)));
        return linkedHashMap;
    }

    private List<Long> queryNoFeatureIssueIds(Set<Long> projectIds,
                                              SearchVO searchVO,
                                              boolean isTreeView,
                                              Map<String, Object> sortMap) {
        setOtherArgsValueByKey(searchVO, "featureNull", "true");
        List<String> list = new ArrayList<>();
        list.add("0");
        setOtherArgsValueByKey(searchVO, "feature", list);
        setOtherArgsValueByKey(searchVO, "epic", list);
        boardAssembler.handleOtherArgs(searchVO);
        List<Long> issueIds = issueService.listByTreeView(projectIds, searchVO, null, sortMap, isTreeView);
        return issueIds;
    }

    private List<Long> queryRootOrNoEpicIssueIds(Set<Long> projectIds,
                                                 SearchVO searchVO,
                                                 boolean isTreeView,
                                                 Map<String, Object> sortMap,
                                                 Long currentId) {
        IssueDTO currentIssue = issueMapper.selectByPrimaryKey(currentId);
        AssertUtilsForCommonException.notNull(currentIssue, "error.gantt.current.issue.not.existed");
        String typeCode = currentIssue.getTypeCode();
        List<Long> issueIds = issueService.listByTreeView(projectIds, searchVO, null, sortMap, isTreeView);
        AssertUtilsForCommonException.notEmpty(issueIds, ERROR_GANTT_MOVE_NULL_DATA);
        List<IssueDTO> issues = issueMapper.selectByIds(StringUtils.join(issueIds, ","));
        Set<Long> epicIds = new HashSet<>();
        List<Long> noEpicIssueIds = new ArrayList<>();
        issues.forEach(issue -> {
            Long epicId = issue.getEpicId();
            if (!ObjectUtils.isEmpty(epicId) && !Objects.equals(0L, epicId)) {
                epicIds.add(epicId);
            } else {
                noEpicIssueIds.add(issue.getIssueId());
            }
        });
        if (agilePluginService != null) {
            epicIds.addAll(agilePluginService.queryNoEpicFeatureIds(issues));
        }
        List<Long> orderedList = new ArrayList<>();
        if (!epicIds.isEmpty() && Boolean.TRUE.equals(searchVO.getGanttDefaultOrder())) {
            String dimension = getDimensionFromSearchVO(searchVO);
            orderedList.addAll(ganttIssueRankMapper.orderByDefaultRank(epicIds, dimension, sortMap));
        }
        List<Long> result = new ArrayList<>(orderedList);
        if (!IssueTypeCode.isFeature(typeCode)) {
            result.addAll(noEpicIssueIds);
        }
        return result;
    }

    private List<Long> queryUnderEpicIssueIds(Set<Long> projectIds,
                                              SearchVO searchVO,
                                              Long instanceId,
                                              String instanceType,
                                              Long currentId,
                                              boolean isTreeView,
                                              Map<String, Object> sortMap) {
        addFilterConditionByInstanceType(searchVO, instanceType, instanceId);
        List<Long> issueIds = issueService.listByTreeView(projectIds, searchVO, null, sortMap, isTreeView);
        IssueDTO issue = issueMapper.selectByPrimaryKey(currentId);
        AssertUtilsForCommonException.notNull(issue, "error.gantt.move.currentId.not.existed");
        String typeCode = issue.getTypeCode();
        if (IssueTypeCode.isFeature(typeCode) && agilePluginService != null) {
            AssertUtilsForCommonException.notEmpty(issueIds, ERROR_GANTT_MOVE_NULL_DATA);
            issueIds = agilePluginService.queryFeatureIdByIssueIdAndEpicId(issueIds, instanceId);
        }
        return issueIds;
    }


    private void addFilterConditionByInstanceType(SearchVO searchVO,
                                                  String instanceType,
                                                  Long instanceId) {
        GanttDimension ganttDimension = GanttDimension.valueOf(instanceType.toUpperCase());
        List<String> instanceIdStrList = new ArrayList<>();
        instanceIdStrList.add(instanceId.toString());
        switch (ganttDimension) {
            case EPIC:
                setOtherArgsValueByKey(searchVO, "epic", instanceIdStrList);
                break;
            case FEATURE:
                setOtherArgsValueByKey(searchVO, "feature", instanceIdStrList);
                break;
            case SPRINT:
                setOtherArgsValueByKey(searchVO, SPRINT, instanceIdStrList);
                break;
            case ASSIGNEE:
                setOtherArgsValueByKey(searchVO, "assigneeId", instanceIdStrList);
                break;
            case TASK:
                break;
            default:
                break;
        }
        boardAssembler.handleOtherArgs(searchVO);
    }

    private void setOtherArgsValueByKey(SearchVO searchVO, String key, Object value) {
        Map<String, Object> otherArgs = searchVO.getOtherArgs();
        if (otherArgs == null) {
            otherArgs = new HashMap<>();
            searchVO.setOtherArgs(otherArgs);
        }
        otherArgs.put(key, value);
    }

    private void addDimensionIfNotExisted(SearchVO searchVO, String dimension) {
        Map<String, Object> searchArgs = searchVO.getSearchArgs();
        if (searchArgs == null) {
            searchArgs = new HashMap<>();
            searchVO.setSearchArgs(searchArgs);
        }
        searchArgs.put("dimension", dimension);
    }

    private void updateGanttIssueRank(Long issueId,
                                      Long organizationId,
                                      Long projectId,
                                      Long instanceId,
                                      String instanceType,
                                      String dimension,
                                      String rank) {
        List<GanttIssueRankDTO> ganttIssueRankList =
                ganttIssueRankMapper.selectByOptions(organizationId, projectId, instanceId, instanceType, dimension, new HashSet<>(Arrays.asList(issueId)));
        if (ganttIssueRankList.isEmpty()) {
            GanttIssueRankDTO dto =
                    buildGanttIssueRank(organizationId, projectId, instanceId, instanceType, dimension, issueId, rank);
            if (ganttIssueRankMapper.insert(dto) != 1) {
                throw new CommonException("error.gantt.update.rank");
            }
        } else {
            GanttIssueRankDTO dto = ganttIssueRankList.get(0);
            dto.setRank(rank);
            if (ganttIssueRankMapper.updateByPrimaryKey(dto) != 1) {
                throw new CommonException("error.gantt.update.rank");
            }
        }
    }

    private GanttIssueRankDTO buildGanttIssueRank(Long organizationId,
                                                  Long projectId,
                                                  Long instanceId,
                                                  String instanceType,
                                                  String dimension,
                                                  Long issueId,
                                                  String rank) {
        GanttIssueRankDTO dto = new GanttIssueRankDTO();
        dto.setOrganizationId(organizationId);
        dto.setProjectId(projectId);
        dto.setInstanceType(instanceType);
        dto.setInstanceId(instanceId);
        dto.setDimension(dimension);
        dto.setIssueId(issueId);
        dto.setRank(rank);
        return dto;
    }

    private void moveValidator(GanttMoveVO ganttMoveVO, String dimension, String instanceType) {
        validateDimension(dimension);
        if (!GanttDimension.contains(instanceType)) {
            throw new CommonException("error.illegal.gantt.instanceType");
        }
        Long previewId = ganttMoveVO.getPreviousId();
        Long nextId = ganttMoveVO.getNextId();
        if (ObjectUtils.isEmpty(previewId) && ObjectUtils.isEmpty(nextId)) {
            throw new CommonException("error.gantt.move.target.id.null");
        }
    }

    @Override
    public Page<GanttChartVO> listByProjectIdAndSearch(Map<Long, ProjectVO> projectMap,
                                                       SearchVO searchVO,
                                                       PageRequest pageRequest,
                                                       Long organizationId,
                                                       boolean orderByRank) {
        if (ObjectUtils.isEmpty(projectMap)) {
            return PageUtil.emptyPage(pageRequest.getPage(), pageRequest.getSize());
        }
        Set<Long> projectIds = projectMap.keySet();
        String dimension = getDimensionFromSearchVO(searchVO);
        validateDimension(dimension);
        validateDisplayFields(searchVO);
        Page<GanttChartVO> emptyPage = PageUtil.emptyPage(pageRequest.getPage(), pageRequest.getSize());
        //设置不查询史诗
        buildIssueType(searchVO);
        String filterSql = getFilterSql(searchVO);
        boardAssembler.handleOtherArgs(searchVO);
        boolean isTreeView =
                Boolean.TRUE.equals(
                        Optional.ofNullable(searchVO.getSearchArgs())
                                .map(x -> x.get("tree"))
                                .orElse(true));
        Map<String, Object> sortMap = new HashMap<>();
        boolean isDefaultOrder = ObjectUtils.isEmpty(pageRequest.getSort());
        boolean ganttDefaultOrder = false;
        if (isDefaultOrder && orderByRank) {
            //无排序时根据rank,issueNum排序
            addGanttDefaultOrder(searchVO, pageRequest);
            ganttDefaultOrder = true;
        }
        processSort(pageRequest, sortMap);
        Page<Long> page = issueService.pagedQueryByTreeView(pageRequest, projectIds, searchVO, filterSql, sortMap, isTreeView);
        List<Long> issueIds = page.getContent();
        Map<Long, Long> issueEpicMap = new HashMap<>();
        Map<Long, IssueDTO> issueFeatureMap = new HashMap<>();
        addEpicIdOrFeatureIds(dimension, issueIds, issueEpicMap, issueFeatureMap, projectIds, projectMap);
        if (!ObjectUtils.isEmpty(issueIds)) {
            Set<Long> childrenIds = new HashSet<>();
            if (isTreeView) {
                List<IssueDTO> childIssues = issueMapper.queryChildrenIdByParentId(issueIds, projectIds, searchVO, filterSql, searchVO.getAssigneeFilterIds(), null);
                childrenIds.addAll(childIssues.stream().map(IssueDTO::getIssueId).collect(Collectors.toSet()));
            }
            issueIds.addAll(childrenIds);
            List<IssueDTO> issueList = issueMapper.selectWithSubByIssueIds(projectIds, issueIds, sortMap, ganttDefaultOrder, dimension);
            List<ObjectSchemeFieldVO> displayFieldCodes = searchVO.getDisplayFields();
            List<GanttChartVO> result = buildGanttList(projectMap, issueIds, issueList, issueEpicMap, issueFeatureMap, displayFieldCodes, organizationId);
            return PageUtils.copyPropertiesAndResetContent(page, result);
        } else {
            return emptyPage;
        }

    }

    private void addEpicIdOrFeatureIds(String dimension,
                                       List<Long> issueIds,
                                       Map<Long, Long> issueEpicMap,
                                       Map<Long, IssueDTO> issueFeatureMap,
                                       Set<Long> projectIds,
                                       Map<Long, ProjectVO> projectMap) {
        if (GanttDimension.isEpic(dimension)
                && !ObjectUtils.isEmpty(issueIds)) {
            List<ProjectVO> programs = queryProgramIds(projectIds);
            boolean belongProgram = (agilePluginService != null && !ObjectUtils.isEmpty(programs));
            issueEpicMap.putAll(issueMapper.listIssueWithEpicId(projectIds, issueIds)
                    .stream()
                    .collect(Collectors.toMap(IssueDTO::getIssueId, IssueDTO::getEpicId)));
            issueIds.addAll(issueEpicMap.values());
            if (belongProgram) {
                programs.forEach(p -> projectMap.put(p.getId(), p));
                issueFeatureMap.putAll(agilePluginService.queryIssueFeature(projectIds, issueIds));
                issueFeatureMap.forEach((issueId, feature) -> issueIds.add(feature.getIssueId()));
            }
        }
    }

    private void addGanttDefaultOrder(SearchVO searchVO, PageRequest pageRequest) {
        Sort.Order projectIdOrder = new Sort.Order(Sort.Direction.DESC, "project_id");
        Sort.Order instanceTypeOrder = new Sort.Order(Sort.Direction.ASC, "instance_type");
        Sort.Order instanceIdOrder = new Sort.Order(Sort.Direction.ASC, "instance_id");
        Sort.Order rankOrder = new Sort.Order(Sort.Direction.ASC, "rank");
        Sort.Order issueNumOrder = new Sort.Order(Sort.Direction.DESC, "issue_num_convert");
        Sort sort = new Sort(projectIdOrder, instanceTypeOrder, instanceIdOrder, rankOrder, issueNumOrder);
        pageRequest.setSort(sort);
        searchVO.setGanttDefaultOrder(true);
    }

    private String getFilterSql(SearchVO searchVO) {
        String filterSql;
        List<Long> quickFilterIds = searchVO.getQuickFilterIds();
        if (!ObjectUtils.isEmpty(quickFilterIds)) {
            filterSql = issueService.getQuickFilter(quickFilterIds);
        } else {
            filterSql = null;
        }
        return filterSql;
    }

    private List<GanttChartVO> buildGanttList(Map<Long, ProjectVO> projectMap,
                                              List<Long> issueIds,
                                              List<IssueDTO> issueList,
                                              Map<Long, Long> issueEpicMap,
                                              Map<Long, IssueDTO> issueFeatureMap,
                                              List<ObjectSchemeFieldVO> displayFields,
                                              Long organizationId) {
        if (ObjectUtils.isEmpty(projectMap) || ObjectUtils.isEmpty(issueList)) {
            return Collections.emptyList();
        }
        Set<Long> projectIds = projectMap.keySet();
        Set<Long> completedStatusIds =
                issueStatusMapper.listCompletedStatus(projectIds)
                        .stream().map(StatusVO::getId).collect(Collectors.toSet());
        Map<Long, IssueSprintDTO> issueSprintMap = queryIssueSprint(projectIds, issueIds);
        Map<String, Object> fieldCodeValues = new HashMap<>();
        Set<Long> systemMemberFieldUserIds = new HashSet<>();
        buildFieldCodeValues(projectIds, issueIds, displayFields, fieldCodeValues, issueList, organizationId, systemMemberFieldUserIds);
        List<IssueTypeVO> issueTypes = issueTypeMapper.selectByProjectIds(organizationId, new ArrayList<>(projectIds));
        Map<Long, Map<Long, IssueTypeVO>> projectIssueTypeMap = new HashMap<>();
        issueTypes.forEach(issueType -> {
            Long projectId = issueType.getProjectId();
            Long issueTypeId = issueType.getId();
            Set<Long> projectIdSet = new HashSet<>();
            if (Objects.equals(0L, projectId)) {
                projectIdSet.addAll(projectIds);
            } else {
                projectIdSet.add(projectId);
            }
            projectIdSet.forEach(id -> {
                Map<Long, IssueTypeVO> issueTypeMap = projectIssueTypeMap.computeIfAbsent(id, x -> new HashMap<>());
                issueTypeMap.put(issueTypeId, issueType);
            });
        });
        Map<Long, StatusVO> statusMap = statusService.queryAllStatusMap(organizationId);
        Set<Long> userIds = new HashSet<>(systemMemberFieldUserIds);
        getUserIdFromIssueList(issueList, userIds);
        Map<Long, UserMessageDTO> usersMap = userService.queryUsersMap(new ArrayList<>(userIds), true);
        List<GanttChartVO> result = new ArrayList<>(issueList.size());
        Set<Long> epicIds = new HashSet<>(issueEpicMap.values());
        Map<Long, IssueDTO> featureMap = new HashMap<>();
        issueFeatureMap.values().forEach(x -> featureMap.put(x.getIssueId(), x));
        Set<Long> featureIds = featureMap.keySet();
        Map<String, String> envMap = lookupValueService.queryMapByTypeCode(FieldCode.ENVIRONMENT);
        Map<Long, PriorityVO> priorityMap = priorityService.queryByOrganizationId(organizationId);
        List<String> fieldCodes = displayFields.stream().map(ObjectSchemeFieldVO::getCode).collect(Collectors.toList());
        issueList.forEach(i -> {
            Long statusId = i.getStatusId();
            Long issueId = i.getIssueId();
            GanttChartVO ganttChart = new GanttChartVO();
            result.add(ganttChart);
            BeanUtils.copyProperties(i, ganttChart);
            boolean completed = completedStatusIds.contains(statusId);
            ganttChart.setCompleted(completed);
            Long thisProjectId = i.getProjectId();
            Map<Long, IssueTypeVO> issueTypeMap = projectIssueTypeMap.get(thisProjectId);
            ganttChart.setIssueTypeVO(issueTypeMap.get(i.getIssueTypeId()));
            ganttChart.setStatusVO(statusMap.get(statusId));
            IssueSprintDTO sprint = issueSprintMap.get(issueId);
            if (!ObjectUtils.isEmpty(sprint)) {
                ganttChart.setSprint(sprint);
            }
            ganttChart.setProjectId(thisProjectId);
            ganttChart.setProject(projectMap.get(thisProjectId));
            ganttChart.setEpicId(issueEpicMap.get(issueId));
            ganttChart.setFeatureId(Optional.ofNullable(issueFeatureMap.get(issueId)).map(IssueDTO::getIssueId).orElse(null));
            ganttChart.setEpicName(i.getEpicName());
            ganttChart.setColor(i.getEpicColor());
            ganttChart.setPriorityVO(priorityMap.get(i.getPriorityId()));
            Long assigneeId = i.getAssigneeId();
            setGanttChartAssignee(usersMap, ganttChart, assigneeId);
            setGanttChartEpicOrFeatureInfo(epicIds, featureIds, i, issueId, ganttChart, thisProjectId, featureMap);
            handlerFieldValue(fieldCodes, fieldCodeValues, ganttChart, i, usersMap, envMap);
            setParentId(ganttChart, i);
        });
        setSubProjectIds(result, issueEpicMap, issueFeatureMap);
        return result;
    }

    private void setSubProjectIds(List<GanttChartVO> result,
                                  Map<Long, Long> issueEpicMap,
                                  Map<Long, IssueDTO> issueFeatureMap) {
        if (ObjectUtils.isEmpty(issueEpicMap)
                && ObjectUtils.isEmpty(issueFeatureMap)) {
            return;
        }
        Map<Long, GanttChartVO> ganttMap = result.stream().collect(Collectors.toMap(GanttChartVO::getIssueId, Function.identity()));
        issueEpicMap.forEach((issueId, epicId) -> addParentSubProjectIds(ganttMap, issueId, epicId));
        issueFeatureMap.forEach((issueId, feature) -> {
            Long featureId = feature.getIssueId();
            addParentSubProjectIds(ganttMap, issueId, featureId);
        });

    }

    private void addParentSubProjectIds(Map<Long, GanttChartVO> ganttMap, Long issueId, Long parentId) {
        GanttChartVO issue = ganttMap.get(issueId);
        GanttChartVO parentIssue = ganttMap.get(parentId);
        if (ObjectUtils.isEmpty(issue) || ObjectUtils.isEmpty(parentIssue)) {
            return;
        }
        Long subProjectId = issue.getProjectId();
        Set<Long> subProjectIds = parentIssue.getSubProjectIds();
        if (subProjectIds == null) {
            subProjectIds = new HashSet<>();
            parentIssue.setSubProjectIds(subProjectIds);
        }
        subProjectIds.add(subProjectId);
    }

    private void setGanttChartEpicOrFeatureInfo(Set<Long> epicIds,
                                                Set<Long> featureIds,
                                                IssueDTO issue,
                                                Long issueId,
                                                GanttChartVO ganttChart,
                                                Long thisProjectId,
                                                Map<Long, IssueDTO> featureMap) {
        if (epicIds.contains(issueId) || featureIds.contains(issueId)) {
            ganttChart.setProgramId(thisProjectId);
            if (featureIds.contains(issueId)) {
                ganttChart.setFeatureName(issue.getSummary());
                IssueDTO feature = featureMap.get(issueId);
                if (!ObjectUtils.isEmpty(feature)) {
                    ganttChart.setFeatureType(feature.getFeatureType());
                }
            }
        }
    }


    private void setGanttChartAssignee(Map<Long, UserMessageDTO> usersMap,
                                       GanttChartVO ganttChart,
                                       Long assigneeId) {
        if (!ObjectUtils.isEmpty(assigneeId)) {
            UserMessageDTO assignee = usersMap.get(assigneeId);
            if (!ObjectUtils.isEmpty(assignee)) {
                ganttChart.setAssignee(assignee);
            }
        }
    }

    private void buildFieldCodeValues(Set<Long> projectIds,
                                      List<Long> issueIds,
                                      List<ObjectSchemeFieldVO> displayFields,
                                      Map<String, Object> fieldCodeValues,
                                      List<IssueDTO> issueList,
                                      Long organizationId,
                                      Set<Long> systemMemberFieldUserIds) {
        // 过滤出自定义字段
        handlerCustomFiledValue(fieldCodeValues, displayFields, projectIds, issueIds, organizationId);
        // 处理预定义字段的值
        systemMemberFieldUserIds.addAll(handlerSystemFieldValue(fieldCodeValues, displayFields, projectIds, issueIds, issueList));
    }

    private Set<Long> handlerSystemFieldValue(Map<String, Object> fieldCodeValues,
                                              List<ObjectSchemeFieldVO> displayFields,
                                              Set<Long> projectIds,
                                              List<Long> issueIds,
                                              List<IssueDTO> issueList) {
        Set<Long> userIds = new HashSet<>();
        displayFields.forEach(field -> {
            String code = field.getCode();
            if (SPECIAL_HANDLER_SYSTEM_FIELD.contains(code)) {
                handlerSystemField(code, issueIds, projectIds, fieldCodeValues, userIds, issueList);
            }
        });
        return userIds;
    }

    private void handlerSystemField(String fieldCode,
                                    List<Long> issueIds,
                                    Set<Long> projectIds,
                                    Map<String, Object> fieldCodeValues,
                                    Set<Long> userIds,
                                    List<IssueDTO> issueList) {
        switch (fieldCode) {
            case FieldCode.LABEL:
                List<LabelIssueRelVO> labelIssueRelVOS = issueLabelMapper.listByIssueIds(projectIds, issueIds);
                if (!CollectionUtils.isEmpty(labelIssueRelVOS)) {
                    Map<Long, List<LabelIssueRelVO>> labelIssueRelGroup = labelIssueRelVOS.stream().collect(Collectors.groupingBy(LabelIssueRelVO::getIssueId));
                    fieldCodeValues.put(fieldCode, labelIssueRelGroup);
                }
                break;
            case FieldCode.COMPONENT:
                List<IssueComponentBriefVO> issueComponentBriefVOS = issueComponentMapper.listByIssueIds(projectIds, issueIds);
                if (!CollectionUtils.isEmpty(issueComponentBriefVOS)) {
                    Map<Long, List<IssueComponentBriefVO>> issueComponentBriefGroup = issueComponentBriefVOS.stream().collect(Collectors.groupingBy(IssueComponentBriefVO::getIssueId));
                    fieldCodeValues.put(fieldCode, issueComponentBriefGroup);
                }
                break;
            case FieldCode.INFLUENCE_VERSION:
                handlerVersionList(fieldCode, projectIds, issueIds, fieldCodeValues);
                break;
            case FieldCode.SPRINT:
                List<IssueSprintVO> issueSprintVOS = issueSprintRelMapper.listByIssueIds(projectIds, issueIds);
                if (!CollectionUtils.isEmpty(issueSprintVOS)) {
                    Map<Long, List<IssueSprintVO>> issueSprintGroup = issueSprintVOS.stream().collect(Collectors.groupingBy(IssueSprintVO::getIssueId));
                    fieldCodeValues.put(fieldCode, issueSprintGroup);
                }
                break;
            case FieldCode.FIX_VERSION:
                handlerVersionList(fieldCode, projectIds, issueIds, fieldCodeValues);
                break;
            case FieldCode.ASSIGNEE:
                handlerUser(userIds, issueList, IssueDTO::getAssigneeId);
                break;
            case FieldCode.REPORTER:
                handlerUser(userIds, issueList, IssueDTO::getReporterId);
                break;
            case CREATE_USER:
                handlerUser(userIds, issueList, IssueDTO::getCreatedBy);
                break;
            case UPDATE_USER:
                handlerUser(userIds, issueList, IssueDTO::getLastUpdatedBy);
                break;
            case FieldCode.PARTICIPANT:
                handlerParticipant(userIds, issueList);
                break;
            case MAIN_RESPONSIBLE_USER:
                handlerUser(userIds, issueList, IssueDTO::getMainResponsibleId);
                break;
            case TAGS:
                if (agilePluginService != null) {
                    agilePluginService.handlerTags(projectIds, issueIds, fieldCodeValues);
                }
                break;
            default:
                break;
        }
    }

    private void handlerParticipant(Set<Long> userIds, List<IssueDTO> issueList) {
        Set<Long> assigneeIds = new HashSet<>();
        for (IssueDTO issueDTO : issueList) {
            List<Long> participantIds = issueDTO.getParticipantIds();
            if (!CollectionUtils.isEmpty(participantIds)) {
                assigneeIds.addAll(participantIds);
            }
        }
        if (!CollectionUtils.isEmpty(assigneeIds)) {
            userIds.addAll(assigneeIds);
        }
    }

    private void handlerVersionList(String fieldCode,
                                    Set<Long> projectIds,
                                    List<Long> issueIds,
                                    Map<String, Object> fieldCodeValues) {
        List<VersionIssueRelVO> versionIssueRelVOS = versionIssueRelMapper.listByIssueIds(projectIds, issueIds, FieldCode.FIX_VERSION.equals(fieldCode) ? "fix" : "influence");
        if (!CollectionUtils.isEmpty(versionIssueRelVOS)) {
            Map<Long, List<VersionIssueRelVO>> issueComponentBriefGroup = versionIssueRelVOS.stream().collect(Collectors.groupingBy(VersionIssueRelVO::getIssueId));
            fieldCodeValues.put(fieldCode, issueComponentBriefGroup);
        }
    }

    private void handlerUser(Set<Long> userIds, List<IssueDTO> issueList, Function<IssueDTO, Long> function) {
        Set<Long> assigneeIds = issueList.stream().map(function).filter(v -> !ObjectUtils.isEmpty(v)).collect(Collectors.toSet());
        if (!CollectionUtils.isEmpty(assigneeIds)) {
            userIds.addAll(assigneeIds);
        }
    }

    private void handlerCustomFiledValue(Map<String, Object> fieldCodeValues,
                                         List<ObjectSchemeFieldVO> displayFields,
                                         Set<Long> projectIds,
                                         List<Long> issueIds,
                                         Long organizationId) {
        if (CollectionUtils.isEmpty(displayFields)) {
            return;
        }
        Map<String, Set<Long>> codeProjectMap = new HashMap<>();
        displayFields.forEach(field -> {
            String fieldCode = field.getCode();
            Set<Long> projectIdSet = codeProjectMap.computeIfAbsent(fieldCode, x -> new HashSet<>());
            Long projectId = field.getProjectId();
            if (!ObjectUtils.isEmpty(projectId)) {
                projectIdSet.add(projectId);
            }
        });
        List<String> fieldCodes = new ArrayList<>(codeProjectMap.keySet());
        List<ObjectSchemeFieldDTO> objectSchemeFieldDTOS = objectSchemeFieldMapper.queryByFieldCodeList(organizationId, projectIds, fieldCodes);
        if (ObjectUtils.isEmpty(objectSchemeFieldDTOS)) {
            return;
        }
        List<ObjectSchemeFieldDTO> customFields = objectSchemeFieldDTOS.stream().filter(v -> Boolean.FALSE.equals(v.getSystem())).collect(Collectors.toList());
        if (ObjectUtils.isEmpty(customFields)) {
            return;
        }
        Map<Long, Map<String, Object>> allIssueFieldMap =
                pageFieldService.queryFieldValueWithIssueIdsForAgileExport(organizationId, new ArrayList<>(projectIds), issueIds, false);
        List<String> customFieldCodes = customFields.stream().map(ObjectSchemeFieldDTO::getCode).collect(Collectors.toList());
        Map<Long, Map<String, Object>> newAllIssueFieldMap = new HashMap<>();
        for (Map.Entry<Long, Map<String, Object>> entry : allIssueFieldMap.entrySet()) {
            Long issueId = entry.getKey();
            Map<String, Object> value = entry.getValue();
            if (CollectionUtils.isEmpty(value)) {
                continue;
            }
            Map<String, Object> filterValue =
                    value
                            .entrySet()
                            .stream()
                            .filter(e -> customFieldCodes.contains(e.getKey()))
                            .collect(Collectors.toMap(
                                    e -> e.getKey(),
                                    e -> e.getValue()
                            ));
            newAllIssueFieldMap.put(issueId, filterValue);
        }
        fieldCodeValues.put("foundationCodeValue", newAllIssueFieldMap);
    }

    private Map<Long, IssueSprintDTO> queryIssueSprint(Set<Long> projectIds,
                                                       List<Long> issueIds) {
        if (ObjectUtils.isEmpty(issueIds)) {
            return Collections.emptyMap();
        }
        Map<Long, IssueSprintDTO> map = new HashMap<>();
        List<String> statusCodes = Arrays.asList("started", "sprint_planning");
        Map<Long, List<IssueSprintDTO>> issueSprintMap =
                issueSprintRelMapper.selectIssueSprintByIds(projectIds, new HashSet<>(issueIds), statusCodes)
                        .stream()
                        .collect(Collectors.groupingBy(IssueSprintDTO::getIssueId));
        issueIds.forEach(issueId -> {
            List<IssueSprintDTO> sprints = issueSprintMap.get(issueId);
            if (ObjectUtils.isEmpty(sprints)) {
                return;
            }
            map.put(issueId, sprints.get(0));
        });
        return map;
    }

    private List<ProjectVO> queryProgramIds(Set<Long> projectIds) {
        Long projectId = new ArrayList<>(projectIds).get(0);
        Long organizationId = ConvertUtil.getOrganizationId(projectId);
        ResponseEntity<List<ProjectVO>> response =
                baseFeignClient.getGroupInfoByEnableProjects(organizationId, projectIds);
        List<ProjectVO> projects = response.getBody();
        if (ObjectUtils.isEmpty(projects)) {
            return Collections.emptyList();
        }
        return projects;
    }

    private void processSort(PageRequest pageRequest, Map<String, Object> sortMap) {
        Sort sort = pageRequest.getSort();
        if (ObjectUtils.isEmpty(sort)) {
            return;
        }
        if (ObjectUtils.isEmpty(sort.getOrderFor(ISSUE_ID))) {
            Sort.Order issueIdOrder = new Sort.Order(Sort.Direction.DESC, ISSUE_ID);
            sort = sort.and(new Sort(issueIdOrder));
        }
        Map<String, String> convertMapping = new HashMap<>();
        convertMapping.put("issueNum", "issue_num_convert");
        String sortSql = PageableHelper.getSortSql(PageUtil.sortResetOrder(sort, null, convertMapping));
        sortMap.put(ORDER_STR, sortSql);
    }

    private void buildIssueType(SearchVO searchVO) {
        List<String> issueTypes = Arrays.asList("story", "bug", "task", "sub_task");
        Map<String, Object> advancedSearchArgs = searchVO.getAdvancedSearchArgs();
        if (advancedSearchArgs == null) {
            advancedSearchArgs = new HashMap<>();
            searchVO.setAdvancedSearchArgs(advancedSearchArgs);
        }
        List<String> typeCodes = new ArrayList<>(issueTypes);
        advancedSearchArgs.put(TYPE_CODES, typeCodes);
    }

    private boolean isSprintEmpty(SearchVO searchVO) {
        Map<String, Object> otherArgs = searchVO.getOtherArgs();
        if (!ObjectUtils.isEmpty(otherArgs)) {
            return ObjectUtils.isEmpty(otherArgs.get(SPRINT));
        }
        return true;
    }

    private void getUserIdFromIssueList(List<IssueDTO> issueList, Set<Long> userIds) {
        for (IssueDTO dto : issueList) {
            if (!ObjectUtils.isEmpty(dto.getReporterId()) && !Objects.equals(0L, dto.getReporterId())) {
                userIds.add(dto.getReporterId());
            }
            if (!ObjectUtils.isEmpty(dto.getAssigneeId()) && !Objects.equals(0L, dto.getAssigneeId())) {
                userIds.add(dto.getAssigneeId());
            }
        }
    }

    private void setParentId(GanttChartVO ganttChartVO,
                             IssueDTO dto) {
        Long relateIssueId = dto.getRelateIssueId();
        Long parentIssueId = dto.getParentIssueId();
        if (!ObjectUtils.isEmpty(relateIssueId) && !Objects.equals(0L, relateIssueId)) {
            ganttChartVO.setParentId(relateIssueId);
            return;
        }
        if (!ObjectUtils.isEmpty(parentIssueId) && !Objects.equals(0L, parentIssueId)) {
            ganttChartVO.setParentId(parentIssueId);
            return;
        }
        Long epicId = ganttChartVO.getEpicId();
        Long featureId = ganttChartVO.getFeatureId();
        if (ObjectUtils.isEmpty(epicId) && IssueTypeCode.isFeature(dto.getTypeCode())) {
            epicId = dto.getEpicId();
        }
        if (!ObjectUtils.isEmpty(featureId)
                && IssueTypeCode.isStory(dto.getTypeCode())) {
            ganttChartVO.setParentId(featureId);
            return;
        }
        if (!ObjectUtils.isEmpty(epicId)
                && (IssueTypeCode.isStory(dto.getTypeCode()) || IssueTypeCode.isFeature(dto.getTypeCode()))) {
            ganttChartVO.setParentId(epicId);
        }
    }

    private void handlerFieldValue(List<String> fieldCodes,
                                   Map<String, Object> fieldCodeValues,
                                   GanttChartVO ganttChartVO,
                                   IssueDTO issueDTO,
                                   Map<Long, UserMessageDTO> usersMap,
                                   Map<String, String> envMap) {
        if (!fieldCodes.contains(FieldCode.STORY_POINTS)) {
            ganttChartVO.setStoryPoints(null);
        }
        if (!fieldCodes.contains(FieldCode.REMAINING_TIME)) {
            ganttChartVO.setRemainingTime(null);
        }
        if (!fieldCodes.contains(FieldCode.CREATION_DATE)) {
            ganttChartVO.setCreationDate(null);
        }
        if (!fieldCodes.contains(FieldCode.LAST_UPDATE_DATE)) {
            ganttChartVO.setLastUpdateDate(null);
        }
        if (fieldCodes.contains(FieldCode.COMPONENT)) {
            Map<Long, List<IssueComponentBriefVO>> componentMap = (Map<Long, List<IssueComponentBriefVO>>) fieldCodeValues.getOrDefault(FieldCode.COMPONENT, new HashMap<>());
            ganttChartVO.setComponents(componentMap.get(issueDTO.getIssueId()));
        }
        if (fieldCodes.contains(FieldCode.FIX_VERSION)) {
            Map<Long, List<VersionIssueRelVO>> fixVersionMap = (Map<Long, List<VersionIssueRelVO>>) fieldCodeValues.getOrDefault(FieldCode.FIX_VERSION, new HashMap<>());
            ganttChartVO.setFixVersion(fixVersionMap.get(issueDTO.getIssueId()));
        }
        if (fieldCodes.contains(FieldCode.INFLUENCE_VERSION)) {
            Map<Long, List<VersionIssueRelVO>> influenceVersionMap = (Map<Long, List<VersionIssueRelVO>>) fieldCodeValues.getOrDefault(FieldCode.INFLUENCE_VERSION, new HashMap<>());
            ganttChartVO.setInfluenceVersion(influenceVersionMap.get(issueDTO.getIssueId()));
        }
        if (fieldCodes.contains(FieldCode.LABEL)) {
            Map<Long, List<LabelIssueRelVO>> labelMap = (Map<Long, List<LabelIssueRelVO>>) fieldCodeValues.getOrDefault(FieldCode.LABEL, new HashMap<>());
            ganttChartVO.setLabels(labelMap.get(issueDTO.getIssueId()));
        }
        if (fieldCodes.contains(FieldCode.SPRINT)) {
            Map<Long, List<IssueSprintVO>> map = (Map<Long, List<IssueSprintVO>>) fieldCodeValues.getOrDefault(FieldCode.SPRINT, new HashMap<>());
            ganttChartVO.setSprints(map.get(issueDTO.getIssueId()));
        }
        if (fieldCodes.contains(CREATE_USER) && !ObjectUtils.isEmpty(issueDTO.getCreatedBy())) {
            ganttChartVO.setCreateUser(usersMap.get(issueDTO.getCreatedBy()));
        }
        if (fieldCodes.contains(UPDATE_USER) && !ObjectUtils.isEmpty(issueDTO.getLastUpdatedBy())) {
            ganttChartVO.setCreateUser(usersMap.get(issueDTO.getLastUpdatedBy()));
        }
        if (fieldCodes.contains(MAIN_RESPONSIBLE_USER) && !ObjectUtils.isEmpty(issueDTO.getMainResponsibleId())) {
            ganttChartVO.setCreateUser(usersMap.get(issueDTO.getMainResponsibleId()));
        }
        // 处理环境字段
        if (fieldCodes.contains(FieldCode.ENVIRONMENT) && !ObjectUtils.isEmpty(issueDTO.getEnvironment())) {
            ganttChartVO.setEnvironment(envMap.get(issueDTO.getEnvironment()));
        }
        // 处理参与人
        if (fieldCodes.contains(PARTICIPANTS) && !CollectionUtils.isEmpty(issueDTO.getParticipantIds())) {
            List<Long> participantIds = issueDTO.getParticipantIds();
            List<UserMessageDTO> participants = new ArrayList<>();
            for (Long participantId : participantIds) {
                UserMessageDTO userMessageDTO = usersMap.get(participantId);
                if (!ObjectUtils.isEmpty(userMessageDTO)) {
                    participants.add(userMessageDTO);
                }
            }
            ganttChartVO.setParticipants(participants);
        }

        if (fieldCodes.contains("tags")) {
            Map<Long, List<TagVO>> tagMap = (Map<Long, List<TagVO>>) fieldCodeValues.getOrDefault("tags", new HashMap<>());
            ganttChartVO.setTags(tagMap.get(issueDTO.getIssueId()));
        }
        Map<Long, Object> customFieldMap = (Map<Long, Object>) fieldCodeValues.getOrDefault("foundationCodeValue", new HashMap<>());
        Map<String, Object> fieldCodeValue = (Map<String, Object>) customFieldMap.getOrDefault(issueDTO.getIssueId(), new HashMap<>());
        if (!CollectionUtils.isEmpty(fieldCodeValue)) {
            ganttChartVO.setFoundationFieldValue(fieldCodeValue);
        }
    }
}
