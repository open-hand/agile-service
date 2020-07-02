package io.choerodon.agile.app.service.impl;

import java.math.BigDecimal;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.time.Duration;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.*;
import java.util.stream.Collectors;

import com.alibaba.fastjson.JSONObject;
import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.app.assembler.IssueAssembler;
import io.choerodon.agile.app.service.ProjectOverviewService;
import io.choerodon.agile.app.service.ReportService;
import io.choerodon.agile.app.service.UserService;
import io.choerodon.agile.infra.dto.SprintDTO;
import io.choerodon.agile.infra.enums.InitIssueType;
import io.choerodon.agile.infra.mapper.IssueMapper;
import io.choerodon.agile.infra.mapper.ReportMapper;
import io.choerodon.agile.infra.mapper.SprintMapper;
import org.hzero.core.base.BaseConstants;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/**
 * @author jiaxu.cui@hand-china.com 2020/6/29 下午3:51
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class ProjectOverviewServiceImpl implements ProjectOverviewService {
    @Autowired
    private UserService userService;
    @Autowired
    private ReportService reportService;
    @Autowired
    private SprintMapper sprintMapper;
    @Autowired
    private IssueMapper issueMapper;
    @Autowired
    private IssueAssembler issueAssembler;
    @Autowired
    private ReportMapper reportMapper;
    @Autowired
    private ModelMapper modelMapper;

    @Override
    @SuppressWarnings("unchecked")
    public UncompletedCountVO selectUncompletedBySprint(Long projectId, Long sprintId) {
        UncompletedCountVO uncompletedCount = new UncompletedCountVO();
        DateFormat df = new SimpleDateFormat(BaseConstants.Pattern.DATETIME);
        DateTimeFormatter ldf = DateTimeFormatter.ofPattern(BaseConstants.Pattern.DATETIME);
        JSONObject jObject;
        jObject = reportService.queryBurnDownCoordinate(projectId, sprintId, ReportServiceImpl.STORY_POINTS);
        uncompletedCount.setStoryPoints(Optional.ofNullable(jObject.get(ReportServiceImpl.COORDINATE))
                .map(map -> ((TreeMap<String, BigDecimal>)map).lastEntry())
                .map(Map.Entry::getValue).orElse(BigDecimal.ZERO));
        jObject = reportService.queryBurnDownCoordinate(projectId, sprintId, ReportServiceImpl.REMAINING_ESTIMATED_TIME);
        uncompletedCount.setRemainingEstimatedTime(Optional.ofNullable(jObject.get(ReportServiceImpl.COORDINATE))
                .map(map -> ((TreeMap<String, BigDecimal>)map).lastEntry())
                .map(Map.Entry::getValue).orElse(BigDecimal.ZERO));
        jObject = reportService.queryBurnDownCoordinate(projectId, sprintId, ReportServiceImpl.ISSUE_COUNT);
        uncompletedCount.setIssueCount(Optional.ofNullable(jObject.get(ReportServiceImpl.COORDINATE))
                .map(map -> ((TreeMap<String, BigDecimal>)map).lastEntry())
                .map(Map.Entry::getValue).orElse(BigDecimal.ZERO));
        SprintDTO sprint = new SprintDTO();
        sprint.setProjectId(projectId);
        sprint.setSprintId(sprintId);
        sprint = sprintMapper.selectOne(sprint);
        if (Objects.isNull(sprint)) {
            return uncompletedCount;
        }
        Date endDate = Objects.nonNull(sprint.getActualEndDate()) ? sprint.getActualEndDate() : sprint.getEndDate();
        Date startDate = sprint.getStartDate();
        Duration remaining = Duration.between(LocalDateTime.now(), LocalDateTime.parse(df.format(endDate),ldf));
        Duration total = Duration.between(LocalDateTime.parse(df.format(startDate),ldf), LocalDateTime.parse(df.format(endDate),ldf));
        uncompletedCount.setRemainingDays(Integer.valueOf(Long.valueOf(remaining.toDays()).toString()));
        if (uncompletedCount.getRemainingDays() < 0){
            uncompletedCount.setRemainingDays(0);
        }
        uncompletedCount.setTotalDays(Integer.valueOf(Long.valueOf(total.toDays()).toString()));
        return uncompletedCount;
    }

    @Override
    public IssueCountVO selectIssueCountBysprint(Long projectId, Long sprintId) {
        List<IssueOverviewVO> issueList = Optional.ofNullable(issueMapper.selectIssueBysprint(projectId, sprintId,
                Collections.singleton(InitIssueType.BUG.getTypeCode()))).orElse(Collections.emptyList());
        // 优先排序set
        Set<Long> priority = issueList.stream().map(IssueOverviewVO::getAssigneeId).collect(Collectors.toSet());
        priority.addAll(issueList.stream().map(IssueOverviewVO::getReporterId).collect(Collectors.toSet()));
        // issueDTO转换IssueCountVO
        return issueAssembler.issueDTOToIssueCountVO(issueList, priority);
    }

    @Override
    public SprintStatisticsVO selectSprintStatistics(Long projectId, Long sprintId) {
        Set<String> statusSet = new HashSet<>();
        statusSet.add(InitIssueType.BUG.getTypeCode());
        statusSet.add(InitIssueType.TASK.getTypeCode());
        statusSet.add(InitIssueType.SUB_TASK.getTypeCode());
        statusSet.add(InitIssueType.STORY.getTypeCode());
        List<IssueOverviewVO> issueList = Optional.ofNullable(issueMapper.selectIssueBysprint(projectId, sprintId, statusSet))
                .orElse(Collections.emptyList());

        return issueAssembler.issueDTOToSprintStatisticsVO(issueList);
    }


}
