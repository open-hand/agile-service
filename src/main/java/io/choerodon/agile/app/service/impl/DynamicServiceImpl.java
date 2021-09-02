package io.choerodon.agile.app.service.impl;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

import io.choerodon.agile.api.vo.DataLogQueryVO;
import io.choerodon.agile.api.vo.IssueTypeVO;
import io.choerodon.agile.api.vo.ProjectVO;
import io.choerodon.agile.api.vo.business.AllDataLogVO;
import io.choerodon.agile.api.vo.business.DataLogVO;
import io.choerodon.agile.api.vo.business.RuleLogRelVO;
import io.choerodon.agile.app.service.*;
import io.choerodon.agile.infra.dto.UserMessageDTO;
import io.choerodon.agile.infra.dto.business.IssueDTO;
import io.choerodon.agile.infra.dto.business.IssueSearchDTO;
import io.choerodon.agile.infra.mapper.DataLogMapper;
import io.choerodon.agile.infra.mapper.FieldDataLogMapper;
import io.choerodon.agile.infra.mapper.IssueMapper;
import io.choerodon.agile.infra.utils.ConvertUtil;
import io.choerodon.core.domain.Page;
import io.choerodon.core.utils.PageUtils;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;

/**
 * @author chihao.ran@hand-china.com
 * @since 2021-03-23
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class DynamicServiceImpl implements DynamicService {

    private static final int MAX_SIZE = 100;

    @Autowired
    private DataLogMapper dataLogMapper;
    @Autowired
    private UserService userService;
    @Autowired
    private FieldDataLogMapper fieldDataLogMapper;
    @Autowired
    private IssueMapper issueMapper;
    @Autowired
    private IssueTypeService issueTypeService;
    @Autowired(required = false)
    private BacklogExpandService backlogExpandService;
    @Autowired(required = false)
    private AgileTriggerService agileTriggerService;

    @Override
    public Page<AllDataLogVO> listLatestOperationInfoByProjectId(Long projectId, DataLogQueryVO dataLogQueryVO, PageRequest pageRequest) {
        if (pageRequest.getSize() > MAX_SIZE) {
            pageRequest.setSize(MAX_SIZE);
        }
        List<AllDataLogVO> allDataLogList = new ArrayList<>();
        boolean isNotFilter = CollectionUtils.isEmpty(dataLogQueryVO.getOtherTypes()) && CollectionUtils.isEmpty(dataLogQueryVO.getTypeIds());
        boolean filterBacklog = !CollectionUtils.isEmpty(dataLogQueryVO.getOtherTypes()) && dataLogQueryVO.getOtherTypes().contains("backlog");
        boolean containIssue = !CollectionUtils.isEmpty(dataLogQueryVO.getTypeIds()) || isNotFilter;
        boolean containBacklog = ((isNotFilter || filterBacklog) && backlogExpandService != null);
        Map<Long, RuleLogRelVO> ruleLogRelMap = new HashMap<>();

        if (containIssue) {
            List<AllDataLogVO> issueDataLogList = dataLogMapper.listIssueDataLogByProjectId(projectId, dataLogQueryVO);
            allDataLogList.addAll(issueDataLogList);
        }
        if (containBacklog) {
            List<AllDataLogVO> backlogDataLogList = backlogExpandService.listBacklogDataLogByProjectId(projectId, dataLogQueryVO);
            allDataLogList.addAll(backlogDataLogList);
        }
        if (containIssue || containBacklog) {
            List<AllDataLogVO> fdDataLogList = fieldDataLogMapper.listFdDataLogByProjectId(projectId, dataLogQueryVO, containBacklog, containIssue);
            allDataLogList.addAll(fdDataLogList);
        }
        if (CollectionUtils.isEmpty(allDataLogList)) {
            return PageUtils.createPageFromList(allDataLogList, pageRequest);
        }
        Page<AllDataLogVO> result = PageUtils.createPageFromList(
                allDataLogList.stream().sorted(
                        Comparator.comparing(AllDataLogVO::getCreationDate, Comparator.reverseOrder())
                                .thenComparing(AllDataLogVO::getLogId, Comparator.reverseOrder())).collect(Collectors.toList()),
                pageRequest
        );
        List<Long> logIds = result.stream().map(AllDataLogVO::getLogId).collect(Collectors.toList());
        if (agileTriggerService != null) {
            List<RuleLogRelVO> ruleLogRelList = agileTriggerService.queryRuleLogRelByLogId(projectId, logIds);
            ruleLogRelMap = ruleLogRelList.stream().collect(Collectors.toMap(RuleLogRelVO::getLogId, Function.identity()));
        }
        setDataLogIssueInfo(result, projectId);
        if (containBacklog) {
            backlogExpandService.setDataLogBacklogInfo(result);
        }
        setDataLogUserAndProjectInfo(result, projectId, ruleLogRelMap);
        appendSummary(result);
        return result;
    }

    private void appendSummary(Page<AllDataLogVO> result) {
        List<String> specialFields = Arrays.asList("Feature Link", "Epic Link", "Epic Child", "Feature Child");
        Set<Long> issueIds = new HashSet<>();
        List<AllDataLogVO> dataLogList = new ArrayList<>();
        result.forEach(x -> {
            if (specialFields.contains(x.getField())) {
                dataLogList.add(x);
                String oldValue = x.getOldValue();
                String newValue = x.getNewValue();
                if (oldValue != null) {
                    issueIds.add(Long.valueOf(oldValue));
                }
                if (newValue != null) {
                    issueIds.add(Long.valueOf(newValue));
                }
            }
        });
        Map<Long, String> summaryMap = new HashMap<>();
        if (!issueIds.isEmpty()) {
            summaryMap.putAll(
                    issueMapper.selectByIds(StringUtils.join(issueIds, ","))
                            .stream()
                            .collect(Collectors.toMap(IssueDTO::getIssueId, IssueDTO::getSummary)));
        }
        dataLogList.forEach(x -> {
            String oldValue = x.getOldValue();
            String newValue = x.getNewValue();
            if (oldValue != null) {
                Long issueId = Long.valueOf(oldValue);
                String summary = summaryMap.get(issueId);
                if (summary != null) {
                    String oldString = x.getOldString();
                    oldString = oldString + ":" + summary;
                    x.setOldString(oldString);
                }
            }
            if (newValue != null) {
                Long issueId = Long.valueOf(newValue);
                String summary = summaryMap.get(issueId);
                if (summary != null) {
                    String newString = x.getNewString();
                    newString = newString + ":" + summary;
                    x.setNewString(newString);
                }
            }
        });
    }

    private void setDataLogUserAndProjectInfo(List<AllDataLogVO> dataLogList, Long projectId, Map<Long, RuleLogRelVO> ruleLogRelMap) {
        ProjectVO project = userService.queryProject(projectId);
        List<Long> createdByIds = dataLogList.stream().map(AllDataLogVO::getCreatedBy).collect(Collectors.toList());
        if (CollectionUtils.isEmpty(createdByIds)) {
            return;
        }
        Map<Long, UserMessageDTO> userMap = userService.queryUsersMap(createdByIds, true);
        dataLogList.forEach(dataLog -> {
            RuleLogRelVO ruleLogRel = ruleLogRelMap.get(dataLog.getLogId());
            if (ruleLogRel != null){
                dataLog.setRuleName(ruleLogRel.getRuleName());
            }
            dataLog.setCreatedByUser(userMap.get(dataLog.getCreatedBy()));
            dataLog.setProjectName(project.getName());
        });
    }

    private void setDataLogIssueInfo(List<AllDataLogVO> issueDataLogList, Long projectId) {
        Long organizationId = ConvertUtil.getOrganizationId(projectId);
        List<Long> issueIds = issueDataLogList
                .stream()
                .filter(dataLog -> "agile_issue".equals(dataLog.getLogType()))
                .map(AllDataLogVO::getInstanceId)
                .collect(Collectors.toList());
        if (CollectionUtils.isEmpty(issueIds)) {
            return;
        }
        Map<Long, IssueTypeVO> issueTypeMap = issueTypeService.listIssueTypeMap(organizationId, projectId);
        List<IssueSearchDTO> issueList = issueMapper.queryIssueByIssueIds(projectId, issueIds);
        Map<Long, IssueSearchDTO> issueMap = issueList.stream().collect(Collectors.toMap(IssueSearchDTO::getIssueId, Function.identity()));
        issueDataLogList.forEach(dataLog -> {
            IssueSearchDTO issue = issueMap.get(dataLog.getInstanceId());
            if(issue == null){
                return;
            }
            dataLog.setNum(issue.getIssueNum());
            dataLog.setSummary(issue.getSummary());
            dataLog.setIssueTypeVO(issueTypeMap.get(issue.getIssueTypeId()));
        });
    }
}
