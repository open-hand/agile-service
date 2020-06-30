package io.choerodon.agile.app.service.impl;

import java.math.BigDecimal;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.time.Duration;
import java.time.LocalDate;
import java.util.*;
import java.util.stream.Collectors;

import com.alibaba.fastjson.JSONObject;
import io.choerodon.agile.api.vo.IssueCountVO;
import io.choerodon.agile.api.vo.UncompletedCountVO;
import io.choerodon.agile.app.service.ProjectOverviewService;
import io.choerodon.agile.app.service.ReportService;
import io.choerodon.agile.infra.dto.IssueDTO;
import io.choerodon.agile.infra.dto.SprintDTO;
import io.choerodon.agile.infra.mapper.IssueMapper;
import io.choerodon.agile.infra.mapper.SprintMapper;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.hzero.core.base.BaseConstants;
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
    private ReportService reportService;
    @Autowired
    private SprintMapper sprintMapper;
    @Autowired
    private IssueMapper issueMapper;

    @Override
    @SuppressWarnings("unchecked")
    public UncompletedCountVO selectUncompletedBySprint(Long projectId, Long sprintId) {
        UncompletedCountVO uncompletedCount = new UncompletedCountVO();
        DateFormat sf = new SimpleDateFormat(BaseConstants.Pattern.DATE);
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
        if (Objects.isNull(sprint)){
            return uncompletedCount;
        }
        Date endDate = Objects.nonNull(sprint.getActualEndDate()) ? sprint.getActualEndDate() : sprint.getEndDate();
        Date startDate = sprint.getStartDate();
        Duration remaining = Duration.between(LocalDate.now(), LocalDate.parse(sf.format(endDate)));
        Duration total = Duration.between(LocalDate.parse(sf.format(startDate)), LocalDate.parse(sf.format(endDate)));
        uncompletedCount.setRemainingDays(Integer.valueOf(Long.valueOf(remaining.toDays()).toString()));
        uncompletedCount.setTotalDays(Integer.valueOf(Long.valueOf(total.toDays()).toString()));
        return uncompletedCount;
    }

    @Override
    public IssueCountVO selectIssueCountBysprint(Long projectId, Long sprintId) {
        IssueCountVO issueCount = new IssueCountVO();
        List<IssueDTO> issueList = Optional.ofNullable(issueMapper.selectIssueBysprint(projectId, sprintId))
                .orElse(Collections.emptyList());
        // 优先排序set
        Set<Long> priority = issueList.stream().map(IssueDTO::getAssigneeId).collect(Collectors.toSet());
        // userid-name Map
        Map<Long, String> userMap = issueList.stream().collect(Collectors.toMap(IssueDTO::getCreatedBy, IssueDTO::getCreatedName));
        priority.addAll(issueList.stream().map(IssueDTO::getReporterId).collect(Collectors.toSet()));
        // 设置提出人list
        issueCount.setCreatedList(sortAndConvertCreated(issueList, priority, userMap));
        // 设置已解决list
        issueCount.setCompletedList(sortAndConvertAssignee(issueList));
        return issueCount;
    }

    /**
     * 过滤掉未完成的issue再进行排序
     * @param issueList 待排序list
     * @return 坐标点list
     */
    private List<Map.Entry<String, Integer>> sortAndConvertAssignee(List<IssueDTO> issueList) {
        return issueList.stream()
                .filter(issue -> BooleanUtils.isTrue(issue.getCompleted()))
                .collect(Collectors.groupingBy(IssueDTO::getAssigneeName)).entrySet()
                .stream()
                .sorted(Comparator.comparing(entry ->entry.getValue().stream().findFirst().map(IssueDTO::getAssigneeId).orElse(-1L)))
                .map(entry -> new ImmutablePair<>(entry.getKey(), entry.getValue().size()))
                .collect(Collectors.toList());
    }


    /**
     * 创建人是经办人或报告人时，排在前面
     * @param issueList 待排序list
     * @param priority 优先set
     * @param userMap 用户map
     * @return 坐标点list
     */
    private List<Map.Entry<String, Integer>> sortAndConvertCreated(List<IssueDTO> issueList, Set<Long> priority, Map<Long, String> userMap) {
        List<Map.Entry<String, Integer>> list = new ArrayList<>(issueList.size());
        Map<Boolean, List<IssueDTO>> group = issueList.stream().collect(Collectors.groupingBy(issue -> priority.contains(issue.getCreatedBy())));
        list.addAll(Optional.ofNullable(group.get(Boolean.TRUE)).orElse(Collections.emptyList())
                .stream().collect(Collectors.groupingBy(IssueDTO::getCreatedBy)).entrySet()
                .stream().sorted(Comparator.comparing(Map.Entry::getKey))
                .map(entry -> new ImmutablePair<>(userMap.get(entry.getKey()), entry.getValue().size()))
                .collect(Collectors.toList()));
        list.addAll(Optional.ofNullable(group.get(Boolean.FALSE)).orElse(Collections.emptyList())
                .stream().collect(Collectors.groupingBy(IssueDTO::getCreatedBy)).entrySet()
                .stream().sorted(Comparator.comparing(Map.Entry::getKey))
                .map(entry -> new ImmutablePair<>(userMap.get(entry.getKey()), entry.getValue().size()))
                .collect(Collectors.toList()));
         return list;
    }
}
