package io.choerodon.agile.app.service.impl;

import java.math.BigDecimal;
import java.util.*;
import java.util.stream.Collectors;

import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.app.assembler.IssueAssembler;
import io.choerodon.agile.app.service.ProjectOverviewService;
import io.choerodon.agile.app.service.ReportService;
import io.choerodon.agile.infra.dto.DataLogDTO;
import io.choerodon.agile.infra.dto.SprintDTO;
import io.choerodon.agile.infra.dto.WorkLogDTO;
import io.choerodon.agile.infra.enums.InitIssueType;
import io.choerodon.agile.infra.mapper.*;
import io.choerodon.agile.infra.utils.ConvertUtil;
import io.choerodon.agile.infra.utils.DateUtil;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.hzero.core.base.BaseConstants;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.Assert;

/**
 * @author jiaxu.cui@hand-china.com 2020/6/29 下午3:51
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class ProjectOverviewServiceImpl implements ProjectOverviewService {
    @Autowired
    private ReportService reportService;
    @Autowired
    private SprintMapper sprintMapper;
    @Autowired
    private IssueMapper issueMapper;
    @Autowired
    private IssueAssembler issueAssembler;
    @Autowired
    private WorkLogMapper workLogMapper;
    @Autowired
    private DataLogMapper dataLogMapper;
    @Autowired
    private WorkCalendarRefMapper workCalendarRefMapper;
    @Autowired
    private DateUtil dateUtil;

    @Override
    public UncompletedCountVO selectUncompletedBySprint(Long projectId, Long sprintId) {
        UncompletedCountVO uncompletedCount = new UncompletedCountVO();
        List<IssueOverviewVO> totalIssueList = selectIssueBysprint(projectId, sprintId);
        List<IssueOverviewVO> remainingIssueList = totalIssueList.stream()
                .filter(issue -> BooleanUtils.isFalse(issue.getCompleted())).collect(Collectors.toList());
        SprintDTO sprint = safeSelectSprint(projectId, sprintId);
        Long organizationId = ConvertUtil.getOrganizationId(projectId);
        if (Objects.isNull(sprint)) {
            return uncompletedCount;
        }
        if (sprint.getEndDate() != null) {
            uncompletedCount.setRemainingDays(dateUtil.getDaysBetweenDifferentDate(new Date(), sprint.getEndDate(),
                    workCalendarRefMapper.queryHolidayBySprintIdAndProjectId(sprint.getSprintId(), sprint.getProjectId()),
                    workCalendarRefMapper.queryWorkBySprintIdAndProjectId(sprint.getSprintId(), sprint.getProjectId()), organizationId));
            uncompletedCount.setTotalDays(dateUtil.getDaysBetweenDifferentDate(sprint.getStartDate(), sprint.getEndDate(),
                    workCalendarRefMapper.queryHolidayBySprintIdAndProjectId(sprint.getSprintId(), sprint.getProjectId()),
                    workCalendarRefMapper.queryWorkBySprintIdAndProjectId(sprint.getSprintId(), sprint.getProjectId()), organizationId));
        }
        if (CollectionUtils.isEmpty(totalIssueList)){
            return uncompletedCount;
        }

        if (!CollectionUtils.isEmpty(remainingIssueList)) {
            uncompletedCount.setRemainingStoryPoints(remainingIssueList.stream()
                    .filter(issue -> Objects.equals(issue.getTypeCode(), InitIssueType.STORY.getTypeCode()))
                    .map(IssueOverviewVO::getStoryPoints).filter(Objects::nonNull).reduce(BigDecimal.ZERO, BigDecimal::add));
            uncompletedCount.setRemainingEstimatedTime(remainingIssueList.stream()
                    .map(IssueOverviewVO::getRemainingTime).filter(Objects::nonNull).reduce(BigDecimal.ZERO, BigDecimal::add));
            uncompletedCount.setRemainingIssueCount(remainingIssueList.size());
        }

        uncompletedCount.setTotalStoryPoints(totalIssueList.stream()
                .filter(issue -> Objects.equals(issue.getTypeCode(), InitIssueType.STORY.getTypeCode()))
                .map(IssueOverviewVO::getStoryPoints).filter(Objects::nonNull).reduce(BigDecimal.ZERO, BigDecimal::add));
        BigDecimal totalSpentWorkTime = sprintMapper.querySpentWorkTimeBySprintId(sprintId, projectId);
        BigDecimal totalRemainingEstimatedTime = totalIssueList.stream()
                .map(IssueOverviewVO::getRemainingTime).filter(Objects::nonNull).reduce(BigDecimal.ZERO, BigDecimal::add);
        uncompletedCount.setTotalEstimatedTime(totalSpentWorkTime.add(totalRemainingEstimatedTime));
        uncompletedCount.setTotalIssueCount(totalIssueList.size());
        return uncompletedCount;
    }

    @Override
    public List<IssueCompletedStatusVO> selectIssueCountBysprint(Long projectId, Long sprintId) {
        List<IssueOverviewVO> issueList = selectIssueBysprint(projectId, sprintId);
        List<IssueOverviewVO> bugList = issueList.stream()
                .filter(bug -> InitIssueType.BUG.getTypeCode().equals(bug.getTypeCode()))
                .collect(Collectors.toList());
        // 优先排序set
        Set<Long> priority = issueList.stream().map(IssueOverviewVO::getAssigneeId).collect(Collectors.toSet());
        priority.addAll(issueList.stream().map(IssueOverviewVO::getReporterId).collect(Collectors.toSet()));
        priority.remove(null);
        priority.remove(0L);
        // issueDTO转换IssueCountVO
        return issueAssembler.issueDTOToIssueCountVO(bugList, priority);
    }

    @Override
    public SprintStatisticsVO selectSprintStatistics(Long projectId, Long sprintId) {
        List<IssueOverviewVO> issueList = selectIssueBysprint(projectId, sprintId);
        SprintStatisticsVO sprintStatisticsVO = issueAssembler.issueDTOToSprintStatisticsVO(issueList);
        sprintStatisticsVO.setSprintId(sprintId);
        return sprintStatisticsVO;
    }

    @Override
    public List<OneJobVO> selectOneJobsBySprint(Long projectId, Long sprintId) {
        SprintDTO sprint = safeSelectSprint(projectId,sprintId);
        Assert.notNull(sprint.getStartDate(), BaseConstants.ErrorCode.DATA_INVALID);
        Assert.notNull(sprint.getEndDate(), BaseConstants.ErrorCode.DATA_INVALID);
        if(!Objects.equals("closed", sprint.getStatusCode())){
            sprint.setActualEndDate(new Date());
        } else {
            Assert.notNull(sprint.getActualEndDate(), BaseConstants.ErrorCode.DATA_INVALID);
            sprint.setActualEndDate(sprint.getEndDate());
        }

        List<IssueOverviewVO> issueList = selectIssueBysprint(projectId, sprintId);
        if (CollectionUtils.isEmpty(issueList)){
            return Collections.emptyList();
        }
        List<WorkLogDTO> workLogList = workLogMapper.selectWorkTimeBySpring(projectId, sprintId,
                sprint.getStartDate(), sprint.getActualEndDate());
        Set<Long> issueIdList = issueList.stream().map(IssueOverviewVO::getIssueId).collect(Collectors.toSet());
        List<DataLogDTO> resolutionLogList = dataLogMapper.selectResolutionIssueBySprint(projectId,
                issueIdList, "resolution", sprint.getStartDate(), sprint.getActualEndDate());
        List<DataLogDTO> assigneeLogList = dataLogMapper.selectResolutionIssueBySprint(projectId,
                issueIdList, "assignee", sprint.getStartDate(), sprint.getActualEndDate());
        return issueAssembler.issueToOneJob(sprint, issueList, workLogList, resolutionLogList, assigneeLogList);
    }

    private List<IssueOverviewVO> selectIssueBysprint(Long projectId, Long sprintId) {
        Set<String> statusSet = new HashSet<>();
        statusSet.add(InitIssueType.BUG.getTypeCode());
        statusSet.add(InitIssueType.TASK.getTypeCode());
        statusSet.add(InitIssueType.SUB_TASK.getTypeCode());
        statusSet.add(InitIssueType.STORY.getTypeCode());
        return Optional.ofNullable(issueMapper.selectIssueBysprint(projectId, sprintId, statusSet))
                .orElse(Collections.emptyList());
    }


    private SprintDTO safeSelectSprint(Long projectId, Long sprintId) {
        SprintDTO sprint = new SprintDTO();
        sprint.setProjectId(projectId);
        sprint.setSprintId(sprintId);
        sprint = sprintMapper.selectOne(sprint);
        return sprint;
    }

}
