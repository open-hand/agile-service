package io.choerodon.agile.app.service.impl;

import java.math.BigDecimal;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.time.Duration;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;

import com.alibaba.fastjson.JSONObject;
import io.choerodon.agile.api.vo.IssueCountVO;
import io.choerodon.agile.api.vo.IssueOverviewVO;
import io.choerodon.agile.api.vo.SprintStatisticsVO;
import io.choerodon.agile.api.vo.UncompletedCountVO;
import io.choerodon.agile.app.assembler.IssueAssembler;
import io.choerodon.agile.app.service.ProjectOverviewService;
import io.choerodon.agile.app.service.ReportService;
import io.choerodon.agile.app.service.UserService;
import io.choerodon.agile.infra.dto.IssueDTO;
import io.choerodon.agile.infra.dto.SprintDTO;
import io.choerodon.agile.infra.enums.InitIssueType;
import io.choerodon.agile.infra.mapper.IssueMapper;
import io.choerodon.agile.infra.mapper.SprintMapper;
import org.apache.commons.collections4.SetUtils;
import org.hzero.core.base.BaseConstants;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import rx.Observable;

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

    @Override
    @SuppressWarnings("unchecked")
    public UncompletedCountVO selectUncompletedBySprint(Long projectId, Long sprintId) {
        UncompletedCountVO uncompletedCount = new UncompletedCountVO();
        DateFormat sf = new SimpleDateFormat(BaseConstants.Pattern.DATETIME);
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
        Duration remaining = Duration.between(LocalDateTime.now(), LocalDate.parse(sf.format(endDate)));
        Duration total = Duration.between(LocalDate.parse(sf.format(startDate)), LocalDate.parse(sf.format(endDate)));
        uncompletedCount.setRemainingDays(Integer.valueOf(Long.valueOf(remaining.toDays()).toString()));
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
