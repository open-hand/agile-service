package io.choerodon.agile.app.service.impl;

import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.app.assembler.BoardAssembler;
import io.choerodon.agile.app.service.GanttChartService;
import io.choerodon.agile.app.service.OrganizationGanttChartService;
import io.choerodon.agile.infra.dto.ObjectSchemeFieldDTO;
import io.choerodon.agile.infra.dto.business.IssueDTO;
import io.choerodon.agile.infra.enums.GanttDimension;
import io.choerodon.agile.infra.enums.IssueTypeCode;
import io.choerodon.agile.infra.enums.ProjectCategory;
import io.choerodon.agile.infra.feign.BaseFeignClient;
import io.choerodon.agile.infra.feign.vo.ProjectCategoryDTO;
import io.choerodon.agile.infra.mapper.IssueMapper;
import io.choerodon.agile.infra.mapper.ObjectSchemeFieldMapper;
import io.choerodon.agile.infra.utils.AssertUtilsForCommonException;
import io.choerodon.agile.infra.utils.PageUtil;
import io.choerodon.agile.infra.utils.SearchVOUtil;
import io.choerodon.core.domain.Page;
import io.choerodon.core.exception.CommonException;
import io.choerodon.core.utils.PageUtils;
import io.choerodon.mybatis.pagehelper.PageHelper;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.ObjectUtils;

import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * @author superlee
 * @since 2021-10-18
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class OrganizationGanttChartServiceImpl implements OrganizationGanttChartService {

    @Autowired
    private GanttChartService ganttChartService;
    @Autowired
    private BaseFeignClient baseFeignClient;
    @Autowired
    private ObjectSchemeFieldMapper objectSchemeFieldMapper;
    @Autowired
    private IssueMapper issueMapper;
    @Autowired
    private BoardAssembler boardAssembler;

    private static final String ERROR_GANTT_DIMENSION_NOT_SUPPORT = "error.gantt.dimension.not.support";


    @Override
    public Page<GanttChartVO> pagedQuery(Long organizationId,
                                         SearchVO searchVO,
                                         PageRequest pageRequest) {
        Long projectId = getTeamProjectId(searchVO);
        AssertUtilsForCommonException.notNull(projectId, "error.gantt.teamProjectIds.null");
        List<ProjectVO> projects = listAgileProjects(organizationId, null, null, null);
        if (ObjectUtils.isEmpty(projects)) {
            return PageUtil.emptyPage(pageRequest.getPage(), pageRequest.getSize());
        }
        Map<Long, ProjectVO> projectMap =
                projects.stream().collect(Collectors.toMap(ProjectVO::getId, Function.identity()));
        ProjectVO project = projectMap.get(projectId);
        if (ObjectUtils.isEmpty(project)) {
            throw new CommonException("error.gantt.illegal.project.id");
        }
        Map<Long, ProjectVO> newProjectMap = new HashMap<>();
        newProjectMap.put(projectId, project);
        return ganttChartService.listByProjectIdAndSearch(newProjectMap, searchVO, pageRequest, organizationId, false);
    }

    @Override
    public List<ProjectVO> listAgileProjects(Long organizationId, String name, String code, String param) {
        Page<ProjectVO> projectPage =
                baseFeignClient
                        .pagedQueryProjects(organizationId, 1, 0, name, code, true, param)
                        .getBody();
        List<ProjectVO> projects = projectPage.getContent();
        projects =
                projects
                        .stream()
                        .filter(x -> {
                            Set<String> categories =
                                    x.getCategories().stream().map(ProjectCategoryDTO::getCode).collect(Collectors.toSet());
                            return categories.contains(ProjectCategory.MODULE_AGILE);
                        }).collect(Collectors.toList());
        return projects;
    }

    @Override
    public List<AgileIssueHeadVO> getIssueHeaderFields(Long organizationId, String schemeCode) {
        List<ProjectVO> projects = listAgileProjects(organizationId, null, null, null);
        if (ObjectUtils.isEmpty(projects)) {
            return Collections.emptyList();
        }
        Map<Long, String> projectNameMap =
                projects.stream().collect(Collectors.toMap(ProjectVO::getId, ProjectVO::getName));
        Set<Long> projectIds = projectNameMap.keySet();
        ObjectSchemeFieldSearchVO search = new ObjectSchemeFieldSearchVO();
        search.setSchemeCode(schemeCode);
        search.setIssueTypeList("agileIssueType");
        List<String> issueTypes = Arrays.asList(
                IssueTypeCode.STORY.value(),
                IssueTypeCode.BUG.value(),
                IssueTypeCode.SUB_TASK.value(),
                IssueTypeCode.TASK.value());
        List<ObjectSchemeFieldDTO> objectSchemeFields =
                objectSchemeFieldMapper
                        .listQuery(organizationId, projectIds, search, issueTypes)
                        .stream()
                        .filter(objectSchemeField -> !objectSchemeField.getSystem())
                        .collect(Collectors.toList());
        List<AgileIssueHeadVO> result = new ArrayList<>();
        objectSchemeFields.forEach(field -> {
            AgileIssueHeadVO backlogHead = new AgileIssueHeadVO();
            backlogHead.setId(field.getId());
            backlogHead.setTitle(field.getName());
            backlogHead.setCode(field.getCode());
            backlogHead.setSortId(field.getCode());
            backlogHead.setFieldType(field.getFieldType());
            Long pId = field.getProjectId();
            if (!ObjectUtils.isEmpty(pId)) {
                backlogHead.setProjectId(pId);
                backlogHead.setProjectName(projectNameMap.get(pId));
            }
            result.add(backlogHead);
        });
        return result;
    }

    @Override
    public List<ObjectSchemeFieldDetailVO> listCustomFields(Long organizationId) {
        List<ProjectVO> projects = listAgileProjects(organizationId, null, null, null);
        if (ObjectUtils.isEmpty(projects)) {
            return Collections.emptyList();
        }
        Map<Long, String> projectNameMap =
                projects.stream().collect(Collectors.toMap(ProjectVO::getId, ProjectVO::getName));
        Set<Long> projectIds = projectNameMap.keySet();
        List<String> issueTypes = Arrays.asList(
                IssueTypeCode.STORY.value(),
                IssueTypeCode.TASK.value(),
                IssueTypeCode.BUG.value(),
                IssueTypeCode.SUB_TASK.value());
        List<ObjectSchemeFieldDetailVO> result =
                objectSchemeFieldMapper.selectFieldByProjectIdsWithoutOptions(organizationId, new ArrayList<>(projectIds), null, issueTypes);
        result.forEach(r -> r.setProjectName(projectNameMap.get(r.getProjectId())));
        return result;
    }

    @Override
    public List<EstimatedTimeConflictVO> queryEstimatedTimeConflict(Long organizationId,
                                                                    SearchVO searchVO) {
        Long projectId = getTeamProjectId(searchVO);
        AssertUtilsForCommonException.notNull(projectId, "error.gantt.teamProjectIds.null");
        String dimension = SearchVOUtil.getDimensionFromSearchVO(searchVO);
        if (!GanttDimension.isAssignee(dimension)) {
            throw new CommonException(ERROR_GANTT_DIMENSION_NOT_SUPPORT);
        }
        SearchVOUtil.setTypeCodes(searchVO, Arrays.asList("story", "bug", "task", "sub_task"));
        String filterSql = ganttChartService.getFilterSql(searchVO);
        boardAssembler.handleOtherArgs(searchVO);
        Set<Long> userIds =
                issueMapper.queryAssigneeIdsBySearchVO(new HashSet<>(Arrays.asList(projectId)), searchVO, filterSql, searchVO.getAssigneeFilterIds());
        if (ObjectUtils.isEmpty(userIds)) {
            return Collections.emptyList();
        }
        List<ProjectVO> projects = listAgileProjects(organizationId, null, null, null);
        List<Long> projectIds = projects.stream().map(ProjectVO::getId).collect(Collectors.toList());
        if (projectIds.isEmpty()) {
            return Collections.emptyList();
        }
        List<IssueDTO> issues = issueMapper.selectConflictEstimatedTime(new HashSet<>(projectIds), userIds, null, null, null, null);
        Map<Long, List<IssueDTO>> assigneeMap = issues.stream().collect(Collectors.groupingBy(IssueDTO::getAssigneeId));
        List<EstimatedTimeConflictVO> result = new ArrayList<>();
        userIds.forEach(userId -> {
            EstimatedTimeConflictVO vo = new EstimatedTimeConflictVO();
            result.add(vo);
            vo.setUserId(userId);
            boolean isConflicted = !ObjectUtils.isEmpty(assigneeMap.get(userId));
            vo.setConflicted(isConflicted);
        });
        return result;
    }

    @Override
    public Page<GanttChartVO> queryEstimatedTimeConflictDetails(Long organizationId,
                                                                SearchVO searchVO,
                                                                Long assigneeId,
                                                                PageRequest pageRequest) {
        List<ProjectVO> projects = listAgileProjects(organizationId, null, null, null);
        Map<Long, ProjectVO> projectMap = projects.stream().collect(Collectors.toMap(ProjectVO::getId, Function.identity()));
        List<Long> projectIds = new ArrayList<>(projectMap.keySet());
        if (ObjectUtils.isEmpty(projectIds)) {
            return PageUtil.emptyPage(pageRequest.getPage(), pageRequest.getSize());
        }
        SearchVOUtil.setTypeCodes(searchVO, Arrays.asList("story", "bug", "task", "sub_task"));
        String filterSql = ganttChartService.getFilterSql(searchVO);
        boardAssembler.handleOtherArgs(searchVO);
        Map<String, Object> sortMap = new HashMap<>();
        ganttChartService.processSort(pageRequest, sortMap);
        Page<IssueDTO> issuePage =
                PageHelper.doPage(pageRequest, () -> issueMapper.selectConflictEstimatedTime(
                        new HashSet<>(projectIds),
                        new HashSet<>(Arrays.asList(assigneeId)),
                        searchVO,
                        filterSql,
                        searchVO.getAssigneeFilterIds(),
                        sortMap));
        List<IssueDTO> issues = issuePage.getContent();
        List<Long> issueIds = issues.stream().map(IssueDTO::getIssueId).collect(Collectors.toList());
        List<IssueDTO> issueList = issueMapper.selectWithSubByIssueIds(new HashSet<>(projectIds), issueIds, sortMap, false, null);
        List<GanttChartVO> result = ganttChartService.buildGanttList(projectMap, issueIds, issueList, Collections.emptyMap(), Collections.emptyMap(), Collections.emptyList(), organizationId);
        return PageUtils.copyPropertiesAndResetContent(issuePage, result);
    }

    private Long getTeamProjectId(SearchVO searchVO) {
        Map<String, Object> searchArgs = searchVO.getSearchArgs();
        if (ObjectUtils.isEmpty(searchArgs)) {
            return null;
        }
        List<String> teamProjectIds = (List<String>) searchArgs.get("teamProjectIds");
        if (ObjectUtils.isEmpty(teamProjectIds)) {
            return null;
        }
        return Long.valueOf(teamProjectIds.get(0));
    }
}
