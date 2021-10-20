package io.choerodon.agile.app.service.impl;

import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.app.service.GanttChartService;
import io.choerodon.agile.app.service.OrganizationGanttChartService;
import io.choerodon.agile.infra.dto.ObjectSchemeFieldDTO;
import io.choerodon.agile.infra.enums.IssueTypeCode;
import io.choerodon.agile.infra.enums.ProjectCategory;
import io.choerodon.agile.infra.feign.BaseFeignClient;
import io.choerodon.agile.infra.feign.vo.ProjectCategoryDTO;
import io.choerodon.agile.infra.mapper.ObjectSchemeFieldMapper;
import io.choerodon.agile.infra.utils.AssertUtilsForCommonException;
import io.choerodon.agile.infra.utils.PageUtil;
import io.choerodon.core.domain.Page;
import io.choerodon.core.exception.CommonException;
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
