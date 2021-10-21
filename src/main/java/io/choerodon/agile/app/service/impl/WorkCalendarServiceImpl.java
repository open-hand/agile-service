package io.choerodon.agile.app.service.impl;

import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.app.service.*;
import io.choerodon.agile.infra.dto.UserMessageDTO;
import io.choerodon.agile.infra.feign.BaseFeignClient;
import io.choerodon.agile.infra.mapper.IssueMapper;
import io.choerodon.core.domain.Page;
import io.choerodon.core.exception.CommonException;
import io.choerodon.core.oauth.DetailsHelper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;

import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * @author zhaotianxin
 * @date 2021-10-11 14:02
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class WorkCalendarServiceImpl implements WorkCalendarService {
    @Autowired
    private IssueService issueService;

    @Autowired
    private IssueMapper issueMapper;

    @Autowired
    private PriorityService priorityService;

    @Autowired
    private UserService userService;

    @Autowired
    private StatusService statusService;

    @Autowired
    private BaseFeignClient baseFeignClient;

    @Override
    public List<WorkItemVO> queryAssigneeParentIssueList(Long organizationId, WorkItemSearchVO workItemSearchVO) {
        List<Long> projectIds = new ArrayList<>();
        Long userId = DetailsHelper.getUserDetails().getUserId();
        handlerProject(organizationId, projectIds, userId, workItemSearchVO);
        // 默认按经办人和参与人筛选
        List<String> assigneeFilter = workItemSearchVO.getAssigneeFilter();
        if (CollectionUtils.isEmpty(assigneeFilter)) {
            assigneeFilter = new ArrayList<>();
            assigneeFilter.add("assignee");
            assigneeFilter.add("participant");
            workItemSearchVO.setAssigneeFilter(assigneeFilter);
        }
        if (CollectionUtils.isEmpty(projectIds)) {
            return new ArrayList<>();
        }
        // 查询所有我经办或者我参与的父任务
        List<WorkItemVO> workItemVOS = issueMapper.listWorkCalenderParentIssue(projectIds, userId, workItemSearchVO);
        if (CollectionUtils.isEmpty(workItemVOS)) {
            return new ArrayList<>();
        }
        List<Long> issueIds = workItemVOS.stream().map(WorkItemVO::getIssueId).collect(Collectors.toList());
        // 统计子任务的完成情况
        List<CountVO> countVOS = issueMapper.countWorkCalenderSubIssueProgress(projectIds, userId, issueIds);
        if (!CollectionUtils.isEmpty(countVOS)) {
            Map<Long, CountVO> countVOMap = countVOS.stream().collect(Collectors.toMap(CountVO::getId, Function.identity()));
            for (WorkItemVO workItemVO : workItemVOS) {
                CountVO countVO = countVOMap.get(workItemVO.getIssueId());
                if (!ObjectUtils.isEmpty(countVO)) {
                    workItemVO.setCountVO(countVO);
                }
            }
        }
        return workItemVOS;
    }

    @Override
    public List<WorkItemVO> queryAssigneeIssueList(Long organizationId, WorkItemSearchVO workItemSearchVO) {
        // 校验查询的时间范围
        checkTimeRange(workItemSearchVO);
        // 默认按经办人和参与人筛选
        List<String> assigneeFilter = workItemSearchVO.getAssigneeFilter();
        if (CollectionUtils.isEmpty(assigneeFilter)) {
            assigneeFilter = new ArrayList<>();
            assigneeFilter.add("assignee");
            assigneeFilter.add("participant");
            workItemSearchVO.setAssigneeFilter(assigneeFilter);
        }
        // 如果有做项目筛选,就直接使用。没有的话，查询组织下有权限的项目
        List<Long> projectIds = new ArrayList<>();
        Long userId = DetailsHelper.getUserDetails().getUserId();
        handlerProject(organizationId, projectIds, userId, workItemSearchVO);
        if (CollectionUtils.isEmpty(projectIds)) {
            return new ArrayList<>();
        }
        List<WorkItemVO> workItemVOS = issueMapper.queryAssigneeIssueList(projectIds, userId, workItemSearchVO);
        if (CollectionUtils.isEmpty(workItemVOS)) {
            return new ArrayList<>();
        }
        // 查询优先级
        Map<Long, PriorityVO> priorityVOMap = priorityService.queryByOrganizationId(organizationId);
        // 查询用户
        List<Long> userIds = workItemVOS.stream()
                .filter(v -> !ObjectUtils.isEmpty(v))
                .map(WorkItemVO::getAssigneeId)
                .collect(Collectors.toList());
        // 查询状态
        List<StatusVO> statusVOS = statusService.queryAllStatus(organizationId);
        Map<Long, StatusVO> statusMap = statusVOS.stream().collect(Collectors.toMap(StatusVO::getId, Function.identity()));
        Map<Long, UserMessageDTO> userMessageDTOMap = new HashMap<>();
        if (!CollectionUtils.isEmpty(userIds)) {
            Map<Long, UserMessageDTO> usersMap = userService.queryUsersMap(userIds, true);
            if (!CollectionUtils.isEmpty(usersMap)) {
                userMessageDTOMap.putAll(usersMap);
            }
        }
        for (WorkItemVO workItemVO : workItemVOS) {
            workItemVO.setPriorityVO(priorityVOMap.get(workItemVO.getPriorityId()));
            workItemVO.setAssignee(userMessageDTOMap.get(workItemVO.getAssigneeId()));
            workItemVO.setStatusVO(statusMap.get(workItemVO.getStatusId()));
        }
        return workItemVOS;
    }

    private void checkTimeRange(WorkItemSearchVO workItemSearchVO) {
        if (ObjectUtils.isEmpty(workItemSearchVO.getStartTime()) || ObjectUtils.isEmpty(workItemSearchVO.getStartTime())) {
            throw new CommonException("error.search.time.not.null");
        }
        if (workItemSearchVO.getStartTime().after(workItemSearchVO.getEndTime())) {
            throw new CommonException("error.search.time.illegal");
        }
        if (diffTime(workItemSearchVO.getStartTime(), workItemSearchVO.getEndTime()) > 31) {
            throw new CommonException("error.search.time.illegal");
        }
    }

    private long diffTime(Date startTime, Date endTime) {
        long diff = endTime.getTime() - startTime.getTime();//这样得到的差值是毫秒级别
        long days = diff / (1000 * 60 * 60 * 24);
        return days;
    }

    private void handlerProject(Long organizationId, List<Long> projectIds, Long userId, WorkItemSearchVO workItemSearchVO){
        if (CollectionUtils.isEmpty(workItemSearchVO.getProjectIds())) {
            // 查询有权限的项目
            Page<ProjectVO> page = baseFeignClient.pagingProjectsByUserId(organizationId, userId, 0, 0, true, "N_AGILE").getBody();
            if (!CollectionUtils.isEmpty(page.getContent())) {
                projectIds.addAll(page.getContent().stream().map(ProjectVO::getId).collect(Collectors.toList()));
            }
        } else {
            projectIds.addAll(workItemSearchVO.getProjectIds());
        }
    }
}
