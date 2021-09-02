package io.choerodon.agile.app.service.impl;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.app.assembler.BoardAssembler;
import io.choerodon.agile.app.service.*;
import io.choerodon.agile.infra.dto.IssueSprintDTO;
import io.choerodon.agile.infra.dto.UserMessageDTO;
import io.choerodon.agile.infra.dto.business.IssueDTO;
import io.choerodon.agile.infra.mapper.IssueMapper;
import io.choerodon.agile.infra.utils.ConvertUtil;
import io.choerodon.agile.infra.utils.PageUtil;
import io.choerodon.core.domain.Page;
import io.choerodon.core.exception.CommonException;
import io.choerodon.core.utils.PageUtils;
import io.choerodon.mybatis.pagehelper.PageHelper;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;
import io.choerodon.mybatis.pagehelper.domain.Sort;
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
    public Page<GanttChartVO> pagedQuery(Long projectId,
                                         SearchVO searchVO,
                                         PageRequest pageRequest) {
        if (isSprintEmpty(searchVO)) {
            throw new CommonException("error.otherArgs.sprint.empty");
        }
        return listByProjectIdAndSearch(projectId, searchVO, pageRequest);
    }

    @Override
    public List<GanttChartVO> listByIds(Long projectId, Set<Long> issueIds) {
        if (ObjectUtils.isEmpty(issueIds)) {
            return new ArrayList<>();
        }
        List<IssueDTO> issueDTOList = issueMapper.selectWithSubByIssueIds(projectId, new ArrayList<>(issueIds), null, false);
        Map<Long, Date> completedDateMap =
                issueMapper.selectActuatorCompletedDateByIssueIds(new ArrayList<>(issueIds), projectId)
                        .stream()
                        .collect(Collectors.toMap(GanttChartVO::getIssueId, GanttChartVO::getActualCompletedDate));
        List<GanttChartVO> result = buildFromIssueDto(issueDTOList, projectId, completedDateMap);
        return result;
    }

    private Page<GanttChartVO> listByProjectIdAndSearch(Long projectId,
                                                        SearchVO searchVO,
                                                        PageRequest pageRequest) {
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
            String orderStr = "issue_num_convert desc";
            boolean isTreeView =
                    Boolean.TRUE.equals(
                            Optional.ofNullable(searchVO.getSearchArgs())
                                    .map(x -> x.get("tree"))
                                    .orElse(true));
            Map<String, Object> sortMap = new HashMap<>();
            sortMap.put("orderStr", orderStr);
            Page<Long> page = issueService.pagedQueryByTreeView(pageRequest, projectId, searchVO, filterSql, sortMap, isTreeView);
            List<Long> issueIds = page.getContent();
            if (!ObjectUtils.isEmpty(issueIds)) {
                Set<Long> childrenIds = new HashSet<>();
                if (isTreeView) {
                    childrenIds.addAll(issueMapper.queryChildrenIdByParentId(issueIds, projectId, searchVO, filterSql, searchVO.getAssigneeFilterIds()));
                }
                HashMap<String, String> order = new HashMap<>(1);
                order.put("issueNum", "issue_num_convert");
                Sort sort = PageUtil.sortResetOrder(pageRequest.getSort(), "ai", order);
                List<IssueDTO> issueDTOList = PageHelper.doSort(sort, () -> issueMapper.selectWithSubByIssueIds(projectId, issueIds, childrenIds, isTreeView));
                issueIds.addAll(childrenIds);
                Map<Long, Date> completedDateMap =
                        issueMapper.selectActuatorCompletedDateByIssueIds(issueIds, projectId)
                                .stream()
                                .collect(Collectors.toMap(GanttChartVO::getIssueId, GanttChartVO::getActualCompletedDate));
                List<GanttChartVO> result = buildFromIssueDto(issueDTOList, projectId, completedDateMap);
                return PageUtils.copyPropertiesAndResetContent(page, result);
            } else {
                return emptyPage;
            }
        } else {
            return emptyPage;
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
            List<IssueSprintDTO> sprints = i.getIssueSprintDTOS();
            if (!ObjectUtils.isEmpty(sprints)) {
                ganttChart.setSprint(sprints.get(0));
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
