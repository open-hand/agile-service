package io.choerodon.agile.app.service.impl;

import io.choerodon.agile.api.vo.GanttChartVO;
import io.choerodon.agile.api.vo.ProjectVO;
import io.choerodon.agile.api.vo.SearchVO;
import io.choerodon.agile.app.service.GanttChartService;
import io.choerodon.agile.app.service.OrganizationGanttChartService;
import io.choerodon.agile.infra.enums.ProjectCategory;
import io.choerodon.agile.infra.feign.BaseFeignClient;
import io.choerodon.agile.infra.feign.vo.ProjectCategoryDTO;
import io.choerodon.agile.infra.utils.PageUtil;
import io.choerodon.core.domain.Page;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.ObjectUtils;

import java.util.List;
import java.util.Map;
import java.util.Set;
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

    @Override
    public Page<GanttChartVO> pagedQuery(Long organizationId,
                                         SearchVO searchVO,
                                         PageRequest pageRequest) {

        Page<ProjectVO> projectPage =
                baseFeignClient
                        .pagedQueryProjects(organizationId, 1, 0, null, null, true, null)
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
        if (ObjectUtils.isEmpty(projects)) {
            return PageUtil.emptyPage(pageRequest.getPage(), pageRequest.getSize());
        }
        Map<Long, ProjectVO> projectMap =
                projects.stream().collect(Collectors.toMap(ProjectVO::getId, Function.identity()));
        return ganttChartService.listByProjectIdAndSearch(projectMap, searchVO, pageRequest, organizationId);
    }
}
