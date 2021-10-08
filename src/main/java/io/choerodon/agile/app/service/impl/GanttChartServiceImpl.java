package io.choerodon.agile.app.service.impl;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.app.assembler.BoardAssembler;
import io.choerodon.agile.app.service.*;
import io.choerodon.agile.infra.dto.GanttIssueRankDTO;
import io.choerodon.agile.infra.dto.IssueSprintDTO;
import io.choerodon.agile.infra.dto.UserMessageDTO;
import io.choerodon.agile.infra.dto.business.IssueDTO;
import io.choerodon.agile.infra.enums.GanttDimension;
import io.choerodon.agile.infra.enums.IssueTypeCode;
import io.choerodon.agile.infra.feign.BaseFeignClient;
import io.choerodon.agile.infra.mapper.*;
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
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.ObjectUtils;

import java.util.*;
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
    private static final String SPRINT = "sprint";

    @Autowired
    private IssueService issueService;
    @Autowired
    private BoardAssembler boardAssembler;
    @Autowired
    private IssueMapper issueMapper;
    @Autowired
    private IssueTypeService issueTypeService;
    @Autowired
    private UserService userService;
    @Autowired
    private StatusService statusService;
    @Autowired
    private ProjectConfigService projectConfigService;
    @Autowired
    private ObjectMapper objectMapper;
    @Autowired
    private IssueStatusMapper issueStatusMapper;
    @Autowired
    private IssueSprintRelMapper issueSprintRelMapper;
    @Autowired(required = false)
    private AgilePluginService agilePluginService;
    @Autowired
    private BaseFeignClient baseFeignClient;
    @Autowired
    private GanttIssueRankMapper ganttIssueRankMapper;

    @Override
    public Page<GanttChartVO> pagedQuery(Long projectId,
                                         SearchVO searchVO,
                                         PageRequest pageRequest) {
        if (isSprintEmpty(searchVO)) {
            throw new CommonException("error.otherArgs.sprint.empty");
        }
        String dimension = getDimensionFromSearchVO(searchVO);
        validateDimension(dimension);
        return listByProjectIdAndSearch(projectId, searchVO, pageRequest, dimension);
    }

    private void validateDimension(String dimension) {
        AssertUtilsForCommonException.notNull(dimension, "error.gantt.dimension.null");
        if (!GanttDimension.contains(dimension)) {
            throw new CommonException("error.illegal.gantt.dimension");
        }
        if (GanttDimension.isFeature(dimension)) {
            throw new CommonException("error.gantt.dimension.not.support");
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
    public List<GanttChartVO> listByIds(Long projectId, Set<Long> issueIds, String dimension) {
        if (ObjectUtils.isEmpty(issueIds)) {
            return new ArrayList<>();
        }
        validateDimension(dimension);
        Set<Long> projectIds = new HashSet<>();
        projectIds.add(projectId);
        List<IssueDTO> issueList = issueMapper.selectWithSubByIssueIds(projectIds, new ArrayList<>(issueIds), null, false, null);
        return buildGanttList(projectId, projectIds, new ArrayList<>(issueIds), issueList, new HashMap<>(), new HashMap<>());
    }

    @Override
    public void move(Long projectId, GanttMoveVO ganttMoveVO) {
        String dimension = ganttMoveVO.getDimension();
        String instanceType = ganttMoveVO.getInstanceType();
        Long instanceId = ganttMoveVO.getInstanceId();
        moveValidator(ganttMoveVO, dimension, instanceType);
        SearchVO searchVO = validateAndProcessSearchVO(projectId, ganttMoveVO);

        SearchVO searchWithRequiredFilter = copyRequiredFilter(searchVO);

        Long previousId = ganttMoveVO.getPreviousId();
        Long nextId = ganttMoveVO.getNextId();
        Long currentId = ganttMoveVO.getCurrentId();
        Long organizationId = ConvertUtil.getOrganizationId(projectId);
        //查询没有额外筛选条件的当前维度的排序
        LinkedHashMap<Long, String> issueWithRankMap =
                queryIssueListByInstanceType(projectId, searchWithRequiredFilter, instanceId, instanceType, dimension);
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
                        initRankIfNull(projectId, dimension, instanceType, instanceId, nextId, organizationId, issueWithRankMap);
            }
            String previousRank =
                    ganttIssueRankMapper.selectMaxPreviousRankOrderByRankAsc(organizationId, projectId, instanceId, instanceType, dimension, nextRank);
            if (StringUtils.isEmpty(previousRank)) {
                currentRank = RankUtil.genPre(nextRank);
            } else {
                currentRank = RankUtil.between(previousRank, nextRank);
            }
        }
        updateGanttRank(currentId, organizationId, projectId, instanceId, instanceType, dimension, currentRank);
    }

    private String initRankIfNull(Long projectId,
                                  String dimension,
                                  String instanceType,
                                  Long instanceId,
                                  Long nextId,
                                  Long organizationId,
                                  LinkedHashMap<Long, String> issueWithRankMap) {
        String minRank = null;
        List<Long> nullRankIssueIds = new ArrayList<>();
        boolean afterNextId = false;
        for (Map.Entry<Long, String> entry : issueWithRankMap.entrySet()) {
            Long issueId = entry.getKey();
            String thisRank = entry.getValue();
            if (issueId.equals(nextId)) {
                afterNextId = true;
            }
            if (afterNextId) {
                if (StringUtils.isEmpty(thisRank)) {
                    nullRankIssueIds.add(issueId);
                } else {
                    minRank = thisRank;
                    break;
                }
            }
        }
        int nullRankSize = nullRankIssueIds.size();
        String[] rankArrays = new String[nullRankSize];
        if (minRank == null) {
            minRank = RankUtil.mid();
        }
        for (int i = 0; i < nullRankSize; i++) {
            String thisRank = RankUtil.genPre(minRank);
            minRank = thisRank;
            rankArrays[nullRankSize - i - 1] = thisRank;
        }
        //rank 升序
        List<String> rankList = Arrays.asList(rankArrays);
        List<GanttIssueRankDTO> insertList = new ArrayList<>();
        for (int i = 0; i < nullRankSize; i++) {
            Long issueId = nullRankIssueIds.get(i);
            String thisRank = rankList.get(i);
            insertList.add(buildGanttIssueRank(organizationId, projectId, instanceId, instanceType, dimension, issueId, thisRank));
        }
        ganttIssueRankMapper.batchInsert(insertList);
        return rankList.get(0);
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

    private SearchVO validateAndProcessSearchVO(Long projectId, GanttMoveVO ganttMoveVO) {
        SearchVO searchVO = ganttMoveVO.getSearchVO();
        boolean illegalIssueTypeId = buildIssueType(searchVO, projectId);
        boolean condition = issueService.handleSearchUser(searchVO, projectId);
        if (illegalIssueTypeId || !condition) {
            throw new CommonException("error.illegal.gantt.searchVO");
        }
        if (isSprintEmpty(searchVO)) {
            throw new CommonException("error.otherArgs.sprint.empty");
        }
        boardAssembler.handleOtherArgs(searchVO);
        return searchVO;
    }

    private LinkedHashMap<Long, String> queryIssueListByInstanceType(Long projectId,
                                                                     SearchVO searchVO,
                                                                     Long instanceId,
                                                                     String instanceType,
                                                                     String dimension) {
        PageRequest pageRequest = new PageRequest(1, 0);
        boolean isTreeView = (boolean) searchVO.getSearchArgs().get("tree");
        addDimensionIfNotExisted(searchVO, dimension);
        addFilterConditionByInstanceType(searchVO, instanceType, instanceId);
        Map<String, Object> sortMap = new HashMap<>();
        addGanttDefaultOrder(searchVO, pageRequest);
        boardAssembler.handleOtherArgs(searchVO);
        processSort(pageRequest, sortMap);
        List<Long> issueIds;
        if (GanttDimension.isTask(instanceType) && !Objects.equals(0L, instanceId)) {
            //子任务拖动，查出子任务
            List<IssueDTO> childIssues = issueMapper.queryChildrenIdByParentId(Arrays.asList(instanceId), projectId, searchVO, null, searchVO.getAssigneeFilterIds(), sortMap);
            issueIds = childIssues.stream().map(IssueDTO::getIssueId).collect(Collectors.toList());
        } else {
            issueIds = issueService.listByTreeView(projectId, searchVO, null, sortMap, isTreeView);
        }
        if (issueIds.isEmpty()) {
            throw new CommonException("error.gantt.move.null.data");
        }
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


    private void addFilterConditionByInstanceType(SearchVO searchVO,
                                                  String instanceType,
                                                  Long instanceId) {
        GanttDimension ganttDimension = GanttDimension.valueOf(instanceType.toUpperCase());
        switch (ganttDimension) {
            case EPIC:
                setValueByKey(searchVO, "epic", instanceId);
                break;
            case FEATURE:
                setValueByKey(searchVO, "feature", instanceId);
                break;
            case SPRINT:
                setValueByKey(searchVO, SPRINT, instanceId);
                break;
            case ASSIGNEE:
                setValueByKey(searchVO, "assigneeId", instanceId);
                break;
            case TASK:
                break;
            default:
                break;
        }
    }

    private void setValueByKey(SearchVO searchVO, String key, Long value) {
        Map<String, Object> otherArgs = searchVO.getOtherArgs();
        if (otherArgs == null) {
            otherArgs = new HashMap<>();
            searchVO.setOtherArgs(otherArgs);
        }
        List<String> values = new ArrayList<>();
        values.add(value.toString());
        otherArgs.put(key, values);
    }

    private void addDimensionIfNotExisted(SearchVO searchVO, String dimension) {
        Map<String, Object> searchArgs = searchVO.getSearchArgs();
        if (searchArgs == null) {
            searchArgs = new HashMap<>();
            searchVO.setSearchArgs(searchArgs);
        }
        searchArgs.put("dimension", dimension);
    }

    private void updateGanttRank(Long issueId,
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

    private Page<GanttChartVO> listByProjectIdAndSearch(Long projectId,
                                                        SearchVO searchVO,
                                                        PageRequest pageRequest,
                                                        String dimension) {
        Page<GanttChartVO> emptyPage = PageUtil.emptyPageInfo(pageRequest.getPage(), pageRequest.getSize());
        //设置不查询史诗
        boolean illegalIssueTypeId = buildIssueType(searchVO, projectId);
        if (illegalIssueTypeId) {
            return emptyPage;
        }
        Boolean condition = issueService.handleSearchUser(searchVO, projectId);
        if (Boolean.TRUE.equals(condition)) {
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
            if (isDefaultOrder) {
                //无排序时根据rank,issueNum排序
                addGanttDefaultOrder(searchVO, pageRequest);
                ganttDefaultOrder = true;
            }
            processSort(pageRequest, sortMap);
            Page<Long> page = issueService.pagedQueryByTreeView(pageRequest, projectId, searchVO, filterSql, sortMap, isTreeView);
            List<Long> issueIds = page.getContent();
            Map<Long, Long> issueEpicMap = new HashMap<>();
            Map<Long, IssueDTO> issueFeatureMap = new HashMap<>();
            Set<Long> projectIds = new HashSet<>();
            projectIds.add(projectId);
            addEpicIdOrFeatureIds(projectId, dimension, issueIds, issueEpicMap, issueFeatureMap, projectIds);
            if (!ObjectUtils.isEmpty(issueIds)) {
                Set<Long> childrenIds = new HashSet<>();
                if (isTreeView) {
                    List<IssueDTO> childIssues = issueMapper.queryChildrenIdByParentId(issueIds, projectId, searchVO, filterSql, searchVO.getAssigneeFilterIds(), null);
                    childrenIds.addAll(childIssues.stream().map(IssueDTO::getIssueId).collect(Collectors.toSet()));
                }
                issueIds.addAll(childrenIds);
                List<IssueDTO> issueList = issueMapper.selectWithSubByIssueIds(projectIds, issueIds, sortMap, ganttDefaultOrder, dimension);
                List<GanttChartVO> result = buildGanttList(projectId, projectIds, issueIds, issueList, issueEpicMap, issueFeatureMap);
                return PageUtils.copyPropertiesAndResetContent(page, result);
            } else {
                return emptyPage;
            }
        } else {
            return emptyPage;
        }
    }

    private void addEpicIdOrFeatureIds(Long projectId,
                                       String dimension,
                                       List<Long> issueIds,
                                       Map<Long, Long> issueEpicMap,
                                       Map<Long, IssueDTO> issueFeatureMap,
                                       Set<Long> projectIds) {
        if (GanttDimension.isEpic(dimension)) {
            Long programId = queryProgramId(projectId);
            boolean belongProgram = (agilePluginService != null && !ObjectUtils.isEmpty(programId));
            issueEpicMap.putAll(issueMapper.listIssueWithEpicId(projectId, issueIds)
                    .stream()
                    .collect(Collectors.toMap(IssueDTO::getIssueId, IssueDTO::getEpicId)));
            issueIds.addAll(issueEpicMap.values());
            if (belongProgram) {
                projectIds.add(programId);
                issueFeatureMap.putAll(agilePluginService.queryIssueFeature(projectId, issueIds));
                issueFeatureMap.forEach((issueId, feature) -> issueIds.add(feature.getIssueId()));
            }
        }
    }

    private void addGanttDefaultOrder(SearchVO searchVO, PageRequest pageRequest) {
        Sort.Order instanceTypeOrder = new Sort.Order(Sort.Direction.ASC, "instance_type");
        Sort.Order instanceIdOrder = new Sort.Order(Sort.Direction.ASC, "instance_id");
        Sort.Order rankOrder = new Sort.Order(Sort.Direction.ASC, "rank");
        Sort.Order issueNumOrder = new Sort.Order(Sort.Direction.DESC, "issue_num_convert");
        Sort sort = new Sort(instanceTypeOrder, instanceIdOrder, rankOrder, issueNumOrder);
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

    private List<GanttChartVO> buildGanttList(Long projectId,
                                              Set<Long> projectIds,
                                              List<Long> issueIds,
                                              List<IssueDTO> issueList,
                                              Map<Long, Long> issueEpicMap,
                                              Map<Long, IssueDTO> issueFeatureMap) {
        if (ObjectUtils.isEmpty(issueList)) {
            return Collections.emptyList();
        }
        Set<Long> completedStatusIds =
                issueStatusMapper.listCompletedStatus(new HashSet<>(Arrays.asList(projectId)))
                        .stream().map(StatusVO::getId).collect(Collectors.toSet());
        Map<Long, IssueSprintDTO> issueSprintMap = queryIssueSprint(projectId, issueIds);
        Map<Long, Date> completedDateMap =
                issueMapper.selectActuatorCompletedDateByIssueIds(issueIds, projectId)
                        .stream()
                        .collect(Collectors.toMap(GanttChartVO::getIssueId, GanttChartVO::getActualCompletedDate));
        Long organizationId = ConvertUtil.getOrganizationId(projectId);
        Map<Long, Map<Long, IssueTypeVO>> projectIssueTypeMap = new HashMap<>();
        projectIds.forEach(pId -> {
            Map<Long, IssueTypeVO> issueTypeMap = issueTypeService.listIssueTypeMap(organizationId, projectId);
            projectIssueTypeMap.put(pId, issueTypeMap);
        });
        Map<Long, StatusVO> statusMap = statusService.queryAllStatusMap(organizationId);
        Set<Long> userIds = new HashSet<>();
        getUserIdFromIssueList(issueList, userIds);
        Map<Long, UserMessageDTO> usersMap = userService.queryUsersMap(new ArrayList<>(userIds), true);
        List<GanttChartVO> result = new ArrayList<>(issueList.size());
        Set<Long> epicIds = new HashSet<>(issueEpicMap.values());
        Set<Long> featureIds = issueFeatureMap.values().stream().map(IssueDTO::getIssueId).collect(Collectors.toSet());
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
            ganttChart.setEpicId(issueEpicMap.get(issueId));
            ganttChart.setFeatureId(Optional.ofNullable(issueFeatureMap.get(issueId)).map(IssueDTO::getIssueId).orElse(null));
            ganttChart.setEpicName(i.getEpicName());
            ganttChart.setColor(i.getEpicColor());
            Long assigneeId = i.getAssigneeId();
            if (!ObjectUtils.isEmpty(assigneeId)) {
                UserMessageDTO assignee = usersMap.get(assigneeId);
                if (!ObjectUtils.isEmpty(assignee)) {
                    ganttChart.setAssignee(assignee);
                }
            }
            Date completedDate = completedDateMap.get(i.getIssueId());
            if (completedDate != null) {
                ganttChart.setActualCompletedDate(completedDate);
            }
            if (epicIds.contains(issueId) || featureIds.contains(issueId)) {
                ganttChart.setProgramId(thisProjectId);
                if (featureIds.contains(issueId)) {
                    ganttChart.setFeatureName(i.getSummary());
                }
            }
            setParentId(ganttChart, i);
        });
        return result;
    }

    private Map<Long, IssueSprintDTO> queryIssueSprint(Long projectId,
                                                       List<Long> issueIds) {
        if (ObjectUtils.isEmpty(issueIds)) {
            return Collections.emptyMap();
        }
        Map<Long, IssueSprintDTO> map = new HashMap<>();
        List<String> statusCodes = Arrays.asList("started", "sprint_planning");
        Map<Long, List<IssueSprintDTO>> issueSprintMap =
                issueSprintRelMapper.selectIssueSprintByIds(projectId, new HashSet<>(issueIds), statusCodes)
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

    private Long queryProgramId(Long projectId) {
        Long organizationId = ConvertUtil.getOrganizationId(projectId);
        ResponseEntity<ProjectVO> response =
                baseFeignClient.getGroupInfoByEnableProject(organizationId, projectId);
        ProjectVO project = response.getBody();
        if (ObjectUtils.isEmpty(project)) {
            return null;
        }
        return project.getId();
    }

    private Sort processSort(PageRequest pageRequest, Map<String, Object> sortMap) {
        Sort sort = pageRequest.getSort();
        if (ObjectUtils.isEmpty(sort.getOrderFor(ISSUE_ID))) {
            Sort.Order issueIdOrder = new Sort.Order(Sort.Direction.DESC, ISSUE_ID);
            sort = sort.and(new Sort(issueIdOrder));
        }
        Map<String, String> convertMapping = new HashMap<>();
        convertMapping.put("issueNum", "issue_num_convert");
        String sortSql = PageableHelper.getSortSql(PageUtil.sortResetOrder(sort, null, convertMapping));
        sortMap.put(ORDER_STR, sortSql);
        return sort;
    }

    private boolean buildIssueType(SearchVO searchVO, Long projectId) {
        List<String> issueTypes = Arrays.asList("story", "bug", "task", "sub_task");
        String key = ISSUE_TYPE_ID;
        List<Long> allowedIssueTypeIds =
                projectConfigService
                        .queryIssueTypesWithStateMachineIdByProjectId(projectId, "agile", false)
                        .stream()
                        .filter(x -> issueTypes.contains(x.getTypeCode()))
                        .map(IssueTypeWithStateMachineIdVO::getId)
                        .collect(Collectors.toList());

        Map<String, Object> advancedSearchArgs = searchVO.getAdvancedSearchArgs();
        if (advancedSearchArgs == null) {
            advancedSearchArgs = new HashMap<>();
            searchVO.setAdvancedSearchArgs(advancedSearchArgs);
        }
        Object issueTypeId = advancedSearchArgs.get(key);
        List<String> list = new ArrayList<>();
        if (ObjectUtils.isEmpty(issueTypeId)) {
            allowedIssueTypeIds.forEach(a -> list.add(a + ""));
        } else {
            List<String> array = objectMapper.convertValue(issueTypeId, new TypeReference<List<String>>() {
            });
            allowedIssueTypeIds.forEach(a -> {
                String idStr = a + "";
                if (array.contains(idStr)) {
                    list.add(idStr);
                }
            });
        }
        advancedSearchArgs.put(key, list);
        return list.isEmpty();
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
        if (!ObjectUtils.isEmpty(featureId)
                && IssueTypeCode.isStory(dto.getTypeCode())) {
            ganttChartVO.setParentId(featureId);
            return;
        }
        if (!ObjectUtils.isEmpty(epicId)
                && (IssueTypeCode.isStory(dto.getTypeCode()) || IssueTypeCode.isFeature(dto.getTypeCode()))) {
            ganttChartVO.setParentId(epicId);
            return;
        }
    }
}
