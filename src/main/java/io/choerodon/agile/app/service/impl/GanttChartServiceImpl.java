package io.choerodon.agile.app.service.impl;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.app.assembler.BoardAssembler;
import io.choerodon.agile.app.service.*;
import io.choerodon.agile.infra.dto.UserMessageDTO;
import io.choerodon.agile.infra.dto.business.IssueDTO;
import io.choerodon.agile.infra.mapper.IssueMapper;
import io.choerodon.agile.infra.utils.ConvertUtil;
import io.choerodon.core.exception.CommonException;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.ObjectUtils;

import java.util.*;
import java.util.stream.Collectors;

/**
 * @author superlee
 * @since 2020-11-24
 */
@Service
public class GanttChartServiceImpl implements GanttChartService {

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

    @Override
    public List<GanttChartVO> listByTask(Long projectId, SearchVO searchVO) {
        if (isSprintEmpty(searchVO)) {
            throw new CommonException("error.otherArgs.sprint.empty");
        }
        List<GanttChartVO> result = listByProjectIdAndSearch(projectId, searchVO, true);
        return toTree(result);
    }

    private List<GanttChartVO> listByProjectIdAndSearch(Long projectId,
                                                        SearchVO searchVO,
                                                        Boolean isTreeView) {
        //设置不查询史诗
        boolean illegalIssueTypeId = buildIssueType(searchVO, projectId);
        if (illegalIssueTypeId) {
            return new ArrayList<>();
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
            String orderStr = "issue_num_convert desc";
            List<IssueDTO> issues = issueMapper.queryIssueIdsListWithSub(projectId, searchVO, filterSql, searchVO.getAssigneeFilterIds(), orderStr, isTreeView);
            List<Long> issueIds = issues.stream().map(IssueDTO::getIssueId).collect(Collectors.toList());
            if (!ObjectUtils.isEmpty(issueIds)) {
                Set<Long> childrenIds = new HashSet<>();
                if (isTreeView) {
                    childrenIds.addAll(issueMapper.queryChildrenIdByParentId(issueIds, projectId, searchVO, filterSql, searchVO.getAssigneeFilterIds()));
                }
                List<IssueDTO> issueDTOList = issueMapper.selectWithSubByIssueIds(issueIds, childrenIds, projectId, isTreeView);
                issueIds.addAll(childrenIds);
                Map<Long, Date> completedDateMap =
                        issueMapper.selectActuatorCompletedDateByIssueIds(issueIds, projectId)
                                .stream()
                                .collect(Collectors.toMap(GanttChartVO::getIssueId, GanttChartVO::getActualCompletedDate));
                return buildFromIssueDto(issueDTOList, projectId, completedDateMap);
            } else {
                return new ArrayList<>();
            }
        } else {
            return new ArrayList<>();
        }
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

    @Override
    public List<GanttChartTreeVO> listByUser(Long projectId, SearchVO searchVO) {
        List<GanttChartVO> ganttChartList = listByProjectIdAndSearch(projectId, searchVO, false);
        List<GanttChartVO> unassigned = new ArrayList<>();
        Map<String, List<GanttChartVO>> map = new HashMap<>();
        ganttChartList.forEach(g -> {
            UserMessageDTO user = g.getAssignee();
            if (ObjectUtils.isEmpty(user)) {
                unassigned.add(g);
            } else {
                String name = user.getName();
                List<GanttChartVO> ganttCharts = map.get(name);
                if (ganttCharts == null) {
                    ganttCharts = new ArrayList<>();
                    map.put(name, ganttCharts);
                }
                ganttCharts.add(g);
            }
        });
        List<GanttChartTreeVO> result = new ArrayList<>();
        map.forEach((k, v) -> {
            GanttChartTreeVO ganttChartTreeVO = new GanttChartTreeVO();
            ganttChartTreeVO.setSummary(k);
            ganttChartTreeVO.setGroup(true);
            ganttChartTreeVO.setChildren(toTree(v));
            result.add(ganttChartTreeVO);
        });
        if (!unassigned.isEmpty()) {
            GanttChartTreeVO unassignedVO = new GanttChartTreeVO();
            unassignedVO.setSummary("未分配");
            unassignedVO.setGroup(true);
            unassignedVO.setChildren(toTree(unassigned));
            result.add(unassignedVO);
        }
        return result;
    }

    private List<GanttChartVO> toTree(List<GanttChartVO> ganttChartList) {
        List<GanttChartVO> result = new ArrayList<>();
        Map<Long, GanttChartVO> map = new HashMap<>();
        List<GanttChartVO> rootNodes = new ArrayList<>();
        ganttChartList.forEach(g -> map.put(g.getIssueId(), g));
        ganttChartList.forEach(g -> {
            Long parentId = g.getParentId();
            if (parentId == null
                    || map.get(parentId) == null) {
                rootNodes.add(g);
            } else {
                GanttChartVO parent = map.get(parentId);
                List<GanttChartVO> children = parent.getChildren();
                if (children == null) {
                    children = new ArrayList<>();
                    parent.setChildren(children);
                }
                children.add(g);
            }
        });
        result.addAll(rootNodes);
        return result;
    }

    private List<GanttChartVO> buildFromIssueDto(List<IssueDTO> issueList,
                                                 Long projectId,
                                                 Map<Long, Date> completedDateMap) {
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
            GanttChartVO ganttChart = new GanttChartVO();
            result.add(ganttChart);
            BeanUtils.copyProperties(i, ganttChart);
            ganttChart.setIssueTypeVO(issueTypeDTOMap.get(i.getIssueTypeId()));
            ganttChart.setStatusVO(statusMap.get(i.getStatusId()));
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
        return result;
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
