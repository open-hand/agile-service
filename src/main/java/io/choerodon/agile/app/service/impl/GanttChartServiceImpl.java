package io.choerodon.agile.app.service.impl;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.api.vo.FeatureForIssueVO;
import io.choerodon.agile.api.vo.business.TagVO;
import io.choerodon.agile.app.assembler.BoardAssembler;
import io.choerodon.agile.app.service.*;
import io.choerodon.agile.infra.dto.IssueSprintDTO;
import io.choerodon.agile.infra.dto.ObjectSchemeFieldDTO;
import io.choerodon.agile.infra.dto.UserMessageDTO;
import io.choerodon.agile.infra.dto.business.IssueDTO;
import io.choerodon.agile.infra.enums.FieldCode;
import io.choerodon.agile.infra.enums.GanttDimension;
import io.choerodon.agile.infra.feign.BaseFeignClient;
import io.choerodon.agile.infra.mapper.*;
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
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;
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

    private static final String CREATE_USER = "createUser";

    private static final String UPDATE_USER = "updateUser";

    private static final String MAIN_RESPONSIBLE_USER = "mainResponsibleUser";

    private static final String TAGS = "tags";

    private static final String[] SPECIAL_HANDLER_SYSTEM_FIELD = {FieldCode.LABEL, FieldCode.COMPONENT, FieldCode.FIX_VERSION,
            FieldCode.INFLUENCE_VERSION, FieldCode.ASSIGNEE, FieldCode.REPORTER, CREATE_USER, UPDATE_USER, MAIN_RESPONSIBLE_USER
            , FieldCode.SPRINT, TAGS};

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


    @Override
    public Page<GanttChartVO> pagedQuery(Long projectId,
                                         SearchVO searchVO,
                                         PageRequest pageRequest,
                                         String dimension) {
        if (isSprintEmpty(searchVO)) {
            throw new CommonException("error.otherArgs.sprint.empty");
        }
        List<String> displayFieldCodes = searchVO.getDisplayFieldCodes();
        if (CollectionUtils.isEmpty(displayFieldCodes)) {
            displayFieldCodes = new ArrayList<>();
        }
        validateDimension(dimension);
        return listByProjectIdAndSearch(projectId, searchVO, pageRequest, dimension, displayFieldCodes);
    }

    private void validateDimension(String dimension) {
        if (!GanttDimension.contains(dimension)) {
            throw new CommonException("error.illegal.gantt.dimension");
        }
        if (GanttDimension.isFeature(dimension)) {
            throw new CommonException("error.gantt.dimension.not.support");
        }
    }

    @Override
    public List<GanttChartVO> listByIds(Long projectId, Set<Long> issueIds, String dimension) {
        if (ObjectUtils.isEmpty(issueIds)) {
            return new ArrayList<>();
        }
        validateDimension(dimension);
        List<IssueDTO> issueList = issueMapper.selectWithSubByIssueIds(projectId, new ArrayList<>(issueIds));
        return buildGanttList(projectId, dimension, new ArrayList<>(issueIds), issueList, new ArrayList<>());
    }

    private Page<GanttChartVO> listByProjectIdAndSearch(Long projectId,
                                                        SearchVO searchVO,
                                                        PageRequest pageRequest,
                                                        String dimension,
                                                        List<String> fieldCodes) {
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
                List<GanttChartVO> result = buildGanttList(projectId, dimension, issueIds, issueList, fieldCodes);
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
                                              List<IssueDTO> issueList,
                                              List<String> fieldCodes) {
        if (ObjectUtils.isEmpty(issueList)) {
            return Collections.emptyList();
        }
        Set<Long> completedStatusIds =
                issueStatusMapper.listCompletedStatus(new HashSet<>(Arrays.asList(projectId)))
                        .stream().map(StatusVO::getId).collect(Collectors.toSet());
        Map<Long, IssueSprintDTO> issueSprintMap = queryIssueSprint(projectId, issueIds);
        Map<Long, IssueEpicVO> epicMap = new HashMap<>();
        Map<Long, FeatureForIssueVO> featureMap = new HashMap<>();
        if (GanttDimension.isEpic(dimension)) {
            queryAdditionalInfo(issueList, epicMap, featureMap, projectId);
        }
        Map<Long, Date> completedDateMap =
                issueMapper.selectActuatorCompletedDateByIssueIds(issueIds, projectId)
                        .stream()
                        .collect(Collectors.toMap(GanttChartVO::getIssueId, GanttChartVO::getActualCompletedDate));
        Map<String, Object> fieldCodeValues = new HashMap<>();
        buildFieldCodeValues(projectId, issueIds, fieldCodes, fieldCodeValues, issueList);
        return buildFromIssueDto(issueList, projectId, completedDateMap, completedStatusIds, issueSprintMap, epicMap, featureMap, fieldCodes, fieldCodeValues);
    }

    private void buildFieldCodeValues(Long projectId, List<Long> issueIds, List<String> fieldCodes, Map<String, Object> fieldCodeValues, List<IssueDTO> issueList) {
         // 过滤出自定义字段
         handlerCustomFiledValue(fieldCodeValues, fieldCodes, projectId, issueIds);
         // 处理预定义字段的值
         handlerSystemFieldValue(fieldCodeValues, fieldCodes, projectId, issueIds, issueList);
    }

    private void handlerSystemFieldValue(Map<String, Object> fieldCodeValues, List<String> fieldCodes, Long projectId, List<Long> issueIds, List<IssueDTO> issueList) {
        for (String fieldCode : fieldCodes) {
            if (Arrays.asList(SPECIAL_HANDLER_SYSTEM_FIELD).contains(fieldCode)) {
                Set<Long> userIds = new HashSet<>();
                handlerSystemField(fieldCode, issueIds, projectId, fieldCodeValues, userIds, issueList);
                fieldCodeValues.put("userIds", userIds);
            }
        }
    }

    private void handlerSystemField(String fieldCode, List<Long> issueIds, Long projectId, Map<String, Object> fieldCodeValues,Set<Long> userIds, List<IssueDTO> issueList) {
      switch (fieldCode){
          case FieldCode.LABEL:
              List<LabelIssueRelVO> labelIssueRelVOS = issueLabelMapper.listByIssueIds(projectId, issueIds);
              if (!CollectionUtils.isEmpty(labelIssueRelVOS)) {
                  Map<Long, List<LabelIssueRelVO>> labelIssueRelGroup = labelIssueRelVOS.stream().collect(Collectors.groupingBy(LabelIssueRelVO::getIssueId));
                  fieldCodeValues.put(fieldCode, labelIssueRelGroup);
              }
              break;
          case FieldCode.COMPONENT:
              List<IssueComponentBriefVO> issueComponentBriefVOS = issueComponentMapper.listByIssueIds(projectId, issueIds);
              if (!CollectionUtils.isEmpty(issueComponentBriefVOS)) {
                  Map<Long, List<IssueComponentBriefVO>> issueComponentBriefGroup = issueComponentBriefVOS.stream().collect(Collectors.groupingBy(IssueComponentBriefVO::getIssueId));
                  fieldCodeValues.put(fieldCode, issueComponentBriefGroup);
              }
              break;
          case FieldCode.INFLUENCE_VERSION:
               handlerVersionList(fieldCode, projectId, issueIds, fieldCodeValues);
              break;
          case FieldCode.SPRINT:
              List<IssueSprintVO> issueSprintVOS = issueSprintRelMapper.listByIssueIds(projectId, issueIds);
              if (!CollectionUtils.isEmpty(issueSprintVOS)) {
                  Map<Long, List<IssueSprintVO>> issueSprintGroup = issueSprintVOS.stream().collect(Collectors.groupingBy(IssueSprintVO::getIssueId));
                  fieldCodeValues.put(fieldCode, issueSprintGroup);
              }
              break;
          case FieldCode.FIX_VERSION:
              handlerVersionList(fieldCode, projectId, issueIds, fieldCodeValues);
              break;
          case FieldCode.ASSIGNEE:
              handlerUser(userIds,issueList , IssueDTO::getAssigneeId);
              break;
          case FieldCode.REPORTER:
              handlerUser(userIds,issueList , IssueDTO::getReporterId);
              break;
          case CREATE_USER:
              handlerUser(userIds,issueList , IssueDTO::getCreatedBy);
              break;
          case UPDATE_USER:
              handlerUser(userIds,issueList , IssueDTO::getLastUpdatedBy);
              break;
          case MAIN_RESPONSIBLE_USER:
              handlerUser(userIds,issueList , IssueDTO::getMainResponsibleId);
              break;
          case TAGS:
               if(agilePluginService != null){
                   agilePluginService.handlerTags(projectId, issueIds, fieldCodeValues);
               }
              break;
          default:
              break;
      }
    }

    private void handlerVersionList(String fieldCode, Long projectId, List<Long> issueIds, Map<String, Object> fieldCodeValues) {
        List<VersionIssueRelVO> versionIssueRelVOS = versionIssueRelMapper.listByIssueIds(projectId, issueIds, FieldCode.FIX_VERSION.equals(fieldCode) ? "fix" : "influence");
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

    private void handlerCustomFiledValue(Map<String, Object> fieldCodeValues, List<String> fieldCodes, Long projectId, List<Long> issueIds) {
        if (CollectionUtils.isEmpty(fieldCodes)) {
            return;
        }
        List<ObjectSchemeFieldDTO> objectSchemeFieldDTOS = objectSchemeFieldMapper.queryByFieldCodeList(ConvertUtil.getOrganizationId(projectId), projectId, fieldCodes);
        if (!CollectionUtils.isEmpty(objectSchemeFieldDTOS)) {
            List<ObjectSchemeFieldDTO> customFields = objectSchemeFieldDTOS.stream().filter(v -> Boolean.FALSE.equals(v.getSystem())).collect(Collectors.toList());
            if (!CollectionUtils.isEmpty(customFields)) {
                Map<Long, Map<String, Object>> allIssueFieldMap = pageFieldService.queryFieldValueWithIssueIdsForAgileExport(ConvertUtil.getOrganizationId(projectId), Arrays.asList(projectId), issueIds, false);
                List<String> customFieldCodes = customFields.stream().map(ObjectSchemeFieldDTO::getCode).collect(Collectors.toList());
                Map<Long, Map<String, Object>> newAllIssueFieldMap = new HashMap<>();
                for (Map.Entry<Long, Map<String, Object>> entry : allIssueFieldMap.entrySet()) {
                    Long key = entry.getKey();
                    Map<String, Object> value = entry.getValue();
                    if (CollectionUtils.isEmpty(value)) {
                        continue;
                    }
                    Map<String, Object> filterValue = value.entrySet().stream()
                            .filter(e -> customFieldCodes.contains(e.getKey()))
                            .collect(Collectors.toMap(
                                    e -> e.getKey(),
                                    e -> e.getValue()
                            ));
                    newAllIssueFieldMap.put(key, filterValue);
                }
                fieldCodeValues.put("foundationCodeValue", newAllIssueFieldMap);
            }
        }
    }

    private void queryAdditionalInfo(List<IssueDTO> issueList,
                                     Map<Long, IssueEpicVO> epicMap,
                                     Map<Long, FeatureForIssueVO> featureMap,
                                     Long projectId) {
        boolean belongProgram = (agilePluginService != null && belongToProgram(projectId));
        if (belongProgram) {
            epicMap.putAll(queryIssueEpic(issueList, null));
            featureMap.putAll(agilePluginService.queryIssueFeature(projectId, issueList));
        } else {
            epicMap.putAll(queryIssueEpic(issueList, projectId));
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
            List<IssueEpicVO> epics = issueMapper.queryIssueEpicByIds(projectId, new ArrayList<>(epicIds));
            Set<Long> projectIds = epics.stream().map(IssueEpicVO::getProjectId).collect(Collectors.toSet());
            Map<Long, Map<Long, IssueTypeVO>> projectIssueTypeMap = new HashMap<>();
            projectIds.forEach(pId -> {
                Long organizationId = ConvertUtil.getOrganizationId(pId);
                Map<Long, IssueTypeVO> issueTypeMap = issueTypeService.listIssueTypeMap(organizationId, projectId);
                projectIssueTypeMap.put(pId, issueTypeMap);
            });
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
                Long thisProjectId = epic.getProjectId();
                Map<Long, IssueTypeVO> issueTypeMap = projectIssueTypeMap.get(thisProjectId);
                if (!ObjectUtils.isEmpty(issueTypeMap)) {
                    IssueTypeVO issueType = issueTypeMap.get(epic.getIssueTypeId());
                    epic.setIssueTypeVO(issueType);
                }
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
                                                 Map<Long, FeatureForIssueVO> featureMap,
                                                 List<String> fieldCodes,
                                                 Map<String, Object> fieldCodeValues) {
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
        Set<Long> otherUserIds = (Set<Long>) fieldCodeValues.getOrDefault("userIds", new HashSet<>());
        userIds.addAll(otherUserIds);
        Map<Long, UserMessageDTO> usersMap = userService.queryUsersMap(new ArrayList<>(userIds), true);
        List<GanttChartVO> result = new ArrayList<>(issueList.size());
        Map<String, String> envMap = lookupValueService.queryMapByTypeCode(FieldCode.ENVIRONMENT);
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
            FeatureForIssueVO feature = featureMap.get(issueId);
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
            handlerFieldValue(fieldCodes, fieldCodeValues, ganttChart, i, usersMap, envMap);
        });
        setSubIssueEpicAndFeature(result);
        return result;
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
