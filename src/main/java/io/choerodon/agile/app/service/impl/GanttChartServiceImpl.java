package io.choerodon.agile.app.service.impl;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.api.vo.IssueFeatureVO;
import io.choerodon.agile.api.vo.business.IssueListFieldKVVO;
import io.choerodon.agile.app.assembler.BoardAssembler;
import io.choerodon.agile.app.service.*;
import io.choerodon.agile.infra.dto.IssueSprintDTO;
import io.choerodon.agile.infra.dto.UserMessageDTO;
import io.choerodon.agile.infra.dto.business.IssueDTO;
import io.choerodon.agile.infra.enums.GanttDimension;
import io.choerodon.agile.infra.feign.BaseFeignClient;
import io.choerodon.agile.infra.mapper.IssueMapper;
import io.choerodon.agile.infra.mapper.IssueSprintRelMapper;
import io.choerodon.agile.infra.mapper.IssueStatusMapper;
import io.choerodon.agile.infra.utils.ConvertUtil;
import io.choerodon.agile.infra.utils.PageUtil;
import io.choerodon.core.domain.Page;
import io.choerodon.core.exception.CommonException;
import io.choerodon.core.utils.PageUtils;
import io.choerodon.core.utils.PageableHelper;
import io.choerodon.mybatis.pagehelper.PageHelper;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;
import io.choerodon.mybatis.pagehelper.domain.Sort;
import org.modelmapper.ModelMapper;
import org.modelmapper.TypeToken;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.util.ObjectUtils;

import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * @author superlee
 * @since 2020-11-24
 */
@Service
public class GanttChartServiceImpl implements GanttChartService {

    private static final String ISSUE_ID = "issueId";

    private static final String ORDER_STR = "orderStr";

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
    private ModelMapper modelMapper;

    @Override
    public Page<GanttChartVO> pagedQuery(Long projectId,
                                         SearchVO searchVO,
                                         PageRequest pageRequest,
                                         String dimension) {
        if (isSprintEmpty(searchVO)) {
            throw new CommonException("error.otherArgs.sprint.empty");
        }
        validateDimension(projectId, dimension);
        return listByProjectIdAndSearch(projectId, searchVO, pageRequest, dimension);
    }

    private void validateDimension(Long projectId, String dimension) {
        if (!GanttDimension.contains(dimension)) {
            throw new CommonException("error.illegal.gantt.dimension");
        }
        boolean belongToProgram = (agilePluginService != null && belongToProgram(projectId));
        if ((!belongToProgram && GanttDimension.isFeature(dimension))
                || (belongToProgram && GanttDimension.isEpic(dimension))) {
            throw new CommonException("error.gantt.dimension.not.support");
        }
    }

    @Override
    public List<GanttChartVO> listByIds(Long projectId, Set<Long> issueIds, String dimension) {
        if (ObjectUtils.isEmpty(issueIds)) {
            return new ArrayList<>();
        }
        validateDimension(projectId, dimension);
        List<IssueDTO> issueList = issueMapper.selectWithSubByIssueIds(projectId, new ArrayList<>(issueIds));
        return buildGanttList(projectId, dimension, new ArrayList<>(issueIds), issueList);
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
            String filterSql;
            List<Long> quickFilterIds = searchVO.getQuickFilterIds();
            if (!ObjectUtils.isEmpty(quickFilterIds)) {
                filterSql = issueService.getQuickFilter(quickFilterIds);
            } else {
                filterSql = null;
            }
            boardAssembler.handleOtherArgs(searchVO);
            boolean isTreeView =
                    Boolean.TRUE.equals(
                            Optional.ofNullable(searchVO.getSearchArgs())
                                    .map(x -> x.get("tree"))
                                    .orElse(true));
            Map<String, Object> sortMap = new HashMap<>();
            Sort sort = processSort(pageRequest, sortMap);
            Page<Long> page = issueService.pagedQueryByTreeView(pageRequest, projectId, searchVO, filterSql, sortMap, isTreeView);
            List<Long> issueIds = page.getContent();
            if (!ObjectUtils.isEmpty(issueIds)) {
                Set<Long> childrenIds = new HashSet<>();
                if (isTreeView) {
                    childrenIds.addAll(issueMapper.queryChildrenIdByParentId(issueIds, projectId, searchVO, filterSql, searchVO.getAssigneeFilterIds()));
                }
                issueIds.addAll(childrenIds);
                List<IssueDTO> issueList = PageHelper.doSort(sort, () -> issueMapper.selectWithSubByIssueIds(projectId, issueIds));
                List<GanttChartVO> result = buildGanttList(projectId, dimension, issueIds, issueList);
                return PageUtils.copyPropertiesAndResetContent(page, result);
            } else {
                return emptyPage;
            }
        } else {
            return emptyPage;
        }
    }

    private List<GanttChartVO> buildGanttList(Long projectId,
                                              String dimension,
                                              List<Long> issueIds,
                                              List<IssueDTO> issueList) {
        if (ObjectUtils.isEmpty(issueList)) {
            return Collections.emptyList();
        }
        Set<Long> completedStatusIds =
                issueStatusMapper.listCompletedStatus(new HashSet<>(Arrays.asList(projectId)))
                        .stream().map(StatusVO::getId).collect(Collectors.toSet());
        Map<Long, IssueSprintDTO> issueSprintMap = queryIssueSprint(projectId, issueIds);
        Map<Long, IssueEpicVO> epicMap = new HashMap<>();
        Map<Long, IssueFeatureVO> featureMap = new HashMap<>();
        queryAdditionalInfo(issueList, epicMap, featureMap, dimension, projectId);
        Map<Long, Date> completedDateMap =
                issueMapper.selectActuatorCompletedDateByIssueIds(issueIds, projectId)
                        .stream()
                        .collect(Collectors.toMap(GanttChartVO::getIssueId, GanttChartVO::getActualCompletedDate));
        return buildFromIssueDto(issueList, projectId, completedDateMap, completedStatusIds, issueSprintMap, epicMap, featureMap);
    }

    private void queryAdditionalInfo(List<IssueDTO> issueList,
                                     Map<Long, IssueEpicVO> epicMap,
                                     Map<Long, IssueFeatureVO> featureMap,
                                     String dimension,
                                     Long projectId) {
        if (GanttDimension.isEpic(dimension)) {
            epicMap.putAll(queryIssueEpic(issueList, projectId));
        } else if (GanttDimension.isFeature(dimension) && agilePluginService != null) {
            epicMap.putAll(queryIssueEpic(issueList, null));
            List<IssueListFieldKVVO> inputList =
                    modelMapper.map(issueList, new TypeToken<List<IssueListFieldKVVO>>() {
                    }.getType());
            agilePluginService.doToIssueListFieldKVDTO(projectId, inputList);
            inputList.forEach(issue -> {
                Long issueId = issue.getIssueId();
                Long featureId = issue.getFeatureId();
                if (ObjectUtils.isEmpty(featureId)) {
                    return;
                }
                IssueFeatureVO feature = new IssueFeatureVO();
                feature.setIssueId(featureId);
                feature.setFeatureName(issue.getFeatureName());
                feature.setFeatureColor(issue.getFeatureColor());
                featureMap.put(issueId, feature);
            });
        }
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

    private boolean belongToProgram(Long projectId) {
        Long organizationId = ConvertUtil.getOrganizationId(projectId);
        ResponseEntity<ProjectVO> response =
                baseFeignClient.getGroupInfoByEnableProject(organizationId, projectId);
        return response.getBody() != null;
    }

    private Map<Long, IssueEpicVO> queryIssueEpic(List<IssueDTO> issueList,
                                                  Long projectId) {
        Map<Long, IssueEpicVO> map = new HashMap<>();
        Set<Long> epicIds =
                issueList
                        .stream()
                        .filter(x -> x.getEpicId() != null && !Objects.equals(0L, x.getEpicId()))
                        .map(IssueDTO::getEpicId)
                        .collect(Collectors.toSet());
        if (!epicIds.isEmpty()) {
            Map<Long, IssueEpicVO> epicMap =
                    issueMapper.queryIssueEpicByIds(projectId, new ArrayList<>(epicIds))
                            .stream()
                            .collect(Collectors.toMap(IssueEpicVO::getIssueId, Function.identity()));
            issueList.forEach(issue -> {
                Long issueId = issue.getIssueId();
                Long epicId = issue.getEpicId();
                if (ObjectUtils.isEmpty(epicId) || Objects.equals(0L, epicId)) {
                    return;
                }
                IssueEpicVO epic = epicMap.get(epicId);
                if (!ObjectUtils.isEmpty(epic)) {
                    map.put(issueId, epic);
                }
            });
        }
        return map;
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
        String key = "issueTypeId";
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
            return ObjectUtils.isEmpty(otherArgs.get("sprint"));
        }
        return true;
    }

    private List<GanttChartVO> buildFromIssueDto(List<IssueDTO> issueList,
                                                 Long projectId,
                                                 Map<Long, Date> completedDateMap,
                                                 Set<Long> completedStatusIds,
                                                 Map<Long, IssueSprintDTO> issueSprintMap,
                                                 Map<Long, IssueEpicVO> epicMap,
                                                 Map<Long, IssueFeatureVO> featureMap) {
        Long organizationId = ConvertUtil.getOrganizationId(projectId);
        Map<Long, IssueTypeVO> issueTypeDTOMap = issueTypeService.listIssueTypeMap(organizationId, projectId);
        Map<Long, StatusVO> statusMap = statusService.queryAllStatusMap(organizationId);
        Set<Long> userIds = new HashSet<>();
        for (IssueDTO dto : issueList) {
            if (!ObjectUtils.isEmpty(dto.getReporterId()) && !Objects.equals(0L, dto.getReporterId())) {
                userIds.add(dto.getReporterId());
            }
            if (!ObjectUtils.isEmpty(dto.getAssigneeId()) && !Objects.equals(0L, dto.getAssigneeId())) {
                userIds.add(dto.getAssigneeId());
            }
        }
        Map<Long, UserMessageDTO> usersMap = userService.queryUsersMap(new ArrayList<>(userIds), true);
        List<GanttChartVO> result = new ArrayList<>(issueList.size());
        issueList.forEach(i -> {
            Long statusId = i.getStatusId();
            Long issueId = i.getIssueId();
            GanttChartVO ganttChart = new GanttChartVO();
            result.add(ganttChart);
            BeanUtils.copyProperties(i, ganttChart);
            boolean completed = completedStatusIds.contains(statusId);
            ganttChart.setCompleted(completed);
            ganttChart.setIssueTypeVO(issueTypeDTOMap.get(i.getIssueTypeId()));
            ganttChart.setStatusVO(statusMap.get(statusId));
            IssueSprintDTO sprint = issueSprintMap.get(issueId);
            if (!ObjectUtils.isEmpty(sprint)) {
                ganttChart.setSprint(sprint);
            }
            IssueEpicVO epic = epicMap.get(issueId);
            if (!ObjectUtils.isEmpty(epic)) {
                ganttChart.setEpic(epic);
            }
            IssueFeatureVO feature = featureMap.get(issueId);
            if (!ObjectUtils.isEmpty(feature)) {
                ganttChart.setFeature(feature);
            }
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
            setParentId(ganttChart, i);
        });
        setSubIssueEpicAndFeature(result);
        return result;
    }

    private void setSubIssueEpicAndFeature(List<GanttChartVO> result) {
        Map<Long, GanttChartVO> ganttMap =
                result.stream().collect(Collectors.toMap(GanttChartVO::getIssueId, Function.identity()));
        result.forEach(gantt -> {
            Long parentId = gantt.getParentId();
            if (ObjectUtils.isEmpty(parentId)) {
                return;
            }
            GanttChartVO parent = ganttMap.get(parentId);
            if (ObjectUtils.isEmpty(parent)) {
                return;
            }
            gantt.setEpic(parent.getEpic());
            gantt.setFeature(parent.getFeature());
        });
    }

    private void setParentId(GanttChartVO ganttChartVO, IssueDTO dto) {
        Long relateIssueId = dto.getRelateIssueId();
        Long parentIssueId = dto.getParentIssueId();
        if (!ObjectUtils.isEmpty(relateIssueId) && !Objects.equals(0L, relateIssueId)) {
            ganttChartVO.setParentId(relateIssueId);
            return;
        }
        if (!ObjectUtils.isEmpty(parentIssueId) && !Objects.equals(0L, parentIssueId)) {
            ganttChartVO.setParentId(parentIssueId);
        }
    }
}
