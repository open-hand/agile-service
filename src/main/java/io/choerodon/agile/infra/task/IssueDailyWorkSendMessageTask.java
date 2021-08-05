package io.choerodon.agile.infra.task;

import io.choerodon.agile.api.vo.IssueDailyWorkVO;
import io.choerodon.agile.api.vo.ProjectMessageVO;
import io.choerodon.agile.app.service.DelayTaskService;
import io.choerodon.agile.infra.dto.UserDTO;
import io.choerodon.agile.infra.feign.BaseFeignClient;
import io.choerodon.agile.infra.feign.NotifyFeignClient;
import io.choerodon.agile.infra.mapper.IssueMapper;
import io.choerodon.asgard.schedule.annotation.JobTask;
import io.choerodon.asgard.schedule.annotation.TimedTask;
import io.choerodon.asgard.schedule.enums.TriggerTypeEnum;
import io.choerodon.core.enums.MessageAdditionalType;
import org.apache.commons.collections.MapIterator;
import org.apache.commons.collections.keyvalue.MultiKey;
import org.apache.commons.collections.map.MultiKeyMap;
import org.hzero.boot.message.entity.MessageSender;
import org.slf4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;

import java.time.Duration;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

import static org.slf4j.LoggerFactory.getLogger;


/**
 * @author huaxin.deng@hand-china.com 2021-08-02 10:01:14
 */
@Component
public class IssueDailyWorkSendMessageTask {

    private static final Logger LOGGER = getLogger(IssueDailyWorkSendMessageTask.class);

    private static final String ISSUE_DAILY_WORK = "ISSUE_DAILY_WORK";
    private static final String PROJECT_NAME = "projectName";
    private static final String USER_NAME = "userName";
    private static final String DELAY_COUNT = "delayCount";
    private static final String UNFINISHED_COUNT = "unfinishedCount";
    private static final String HTML_TABLE = "htmlTable";
    private static final String BACKSLASH_TR = "</tr>";
    private static final String BACKSLASH_TD = "</td>";
    private static final String BACKSLASH_SPAN = "</span>";
    private static final String BACKSLASH_TABLE = "</table>";
    private static final String TR_TAG_SINGLE = "<tr style = \"background-color: #FFFFFF; height: 40px; line-height: 25px;\">";
    private static final String TR_TAG_DOUBLE = "<tr style = \"background-color: #F5F5FC; height: 40px; line-height: 25px;\">";
    private static final String RED_SPAN = "<span style=\"" +
            "color: #F5222D; " +
            "font-size: 12px; " +
            "font-family: PingFangSC-Regular; " +
            "border: 1px solid red; " +
            "border-radius: 2px; " +
            "padding: 1px 3px; " +
            "white-space:nowrap;\">";
    private static final String TABLE_TAG = "<table style=\"" +
            "width: 100%;" +
            "font-size: 10px;" +
            "table-layout: fixed;" +
            "border-collapse: collapse;" +
            "border-spacing: 0;" +
            "empty-cells: show;\">";
    private static final String TABLE_HEAD = "<thead style=\"background-color: #F3F6FE;color: #000;text-align: left;vertical-align: bottom;\">" +
            "<tr style = \"height: 30px;\">" +
            "<th style=\"word-break: break-all;color: #0F1358;border-width: 0 0 0 1px;font-size: 13px;line-height: 20px;margin: 0;overflow: visible;padding: 5px 5px;\" width=\"2%\"></th>" +
            "<th style=\"word-break: break-all;color: #0F1358;border-width: 0 0 0 1px;font-size: 13px;line-height: 20px;margin: 0;overflow: visible;padding: 5px 5px;\" width=\"53%\">概要</th>" +
            "<th style=\"word-break: break-all;color: #0F1358;border-width: 0 0 0 1px;font-size: 13px;line-height: 20px;margin: 0;overflow: visible;padding: 5px 5px;\" width=\"15%\">编号</th>" +
            "<th style=\"word-break: break-all;color: #0F1358;border-width: 0 0 0 1px;font-size: 13px;line-height: 20px;margin: 0;overflow: visible;padding: 5px 5px;\" width=\"15%\">优先级</th>" +
            "<th style=\"word-break: break-all;color: #0F1358;border-width: 0 0 0 1px;font-size: 13px;line-height: 20px;margin: 0;overflow: visible;padding: 5px 5px;\" width=\"15%\">状态</th>" +
            BACKSLASH_TR +
            "</thead>";
    private static final String TD_TAG = "<td style=\"" +
            "word-break: break-all;" +
            "border-width: 0 0 0 1px;" +
            "font-size: 13px;" +
            "margin: 0;" +
            "overflow: visible;" +
            "padding: 5px 5px;\">";

    private static final String TODO_COLOR = "#FFB100";
    private static final String DOING_COLOR = "#4D90FE";
    private static final String DONE_COLOR = "#00BFA5";
    private static final String STATUS_TODO = "todo";
    private static final String STATUS_DOING = "doing";
    private static final String STATUS_DONE = "done";

    private static final Map<String, String> STATUS_COLOR_MAP = new HashMap<>();

    static {
        STATUS_COLOR_MAP.put(STATUS_TODO, TODO_COLOR);
        STATUS_COLOR_MAP.put(STATUS_DOING, DOING_COLOR);
        STATUS_COLOR_MAP.put(STATUS_DONE, DONE_COLOR);
    }

    @Autowired
    private NotifyFeignClient notifyFeignClient;
    @Autowired
    private BaseFeignClient baseFeignClient;
    @Autowired
    private DelayTaskService delayTaskService;
    @Autowired
    private IssueMapper issueMapper;

    @Value("${services.domain.url}")
    private String domainUrl;

    @JobTask(
            maxRetryCount = 3,
            code = "issueDailyWorkSendMessage",
            description = "每日工作提醒"
    )
    @TimedTask(
            name = "issueDailyWorkSendMessage",
            description = "每日工作提醒",
            oneExecution = false,
            params = {},
            triggerType = TriggerTypeEnum.CRON_TRIGGER,
            cronExpression = "0 0 4 * * ? "
    )
    public void run(Map<String, Object> map) {
        LOGGER.info("===> 开始执行每日工作提醒发送消息定时任务");
        Map<Long, ProjectMessageVO> projectMap = delayTaskService.listEnabledMsgProjects(ISSUE_DAILY_WORK);
        if (ObjectUtils.isEmpty(projectMap)) {
            LOGGER.info("===> 没有配置发送消息的项目，每日工作提醒发送消息定时任务完成");
            return;
        }
        List<MessageSender> messageSenders = new ArrayList<>();

        processSenders(messageSenders, projectMap);
        if (!CollectionUtils.isEmpty(messageSenders)) {
            int step = 500;
            for (int i = 0; i < messageSenders.size(); i += step) {
                int end = i + step;
                if (end >= messageSenders.size()) {
                    end = messageSenders.size();
                }
                List<MessageSender> messageSenderList = messageSenders.subList(i, end);
                notifyFeignClient.batchSendMessage(messageSenderList);
            }
        }
        LOGGER.info("===> 每日工作提醒发送消息定时任务完成");
    }

    private void processSenders(List<MessageSender> messageSenders,
                                Map<Long, ProjectMessageVO> projectMap) {
        Set<Long> projectIds = projectMap.keySet();
        List<IssueDailyWorkVO> issueList = selectDailyWorkIssues(projectIds);
        if (CollectionUtils.isEmpty(issueList)) {
            return;
        }
        Set<Long> userIds = issueList.stream().map(IssueDailyWorkVO::getAssigneeId).collect(Collectors.toSet());
        if (CollectionUtils.isEmpty(userIds)) {
            return;
        }

        Map<Long, UserDTO> userMap = getUserMap(userIds);
        Map<Long, IssueDailyWorkVO> issueMap = issueList.stream().collect(Collectors.toMap(IssueDailyWorkVO::getIssueId, Function.identity()));

        //key1 projectId, key2 userId, value List<Long>保持排序
        MultiKeyMap multiKeyMap = getMultiKeyMap(issueList);
        MapIterator mapIterator = multiKeyMap.mapIterator();
        while(mapIterator.hasNext()) {
            MultiKey multiKey = (MultiKey) mapIterator.next();
            Long projectId = (Long) multiKey.getKey(0);
            Long userId = (Long) multiKey.getKey(1);

            List<Long> issueIds = (List<Long>) mapIterator.getValue();
            List<IssueDailyWorkVO> list = new ArrayList<>();
            issueIds.forEach(v -> list.add(issueMap.get(v)));
            if (!CollectionUtils.isEmpty(list)) {
                UserDTO user = userMap.get(userId);
                if (ObjectUtils.isEmpty(user)) {
                    continue;
                }
                Map<String, String> paramMap = buildParamMap(list, projectMap.get(projectId), user);
                messageSenders.add(getMessageSender(paramMap, user, projectId));
            }
        }
    }

    private List<IssueDailyWorkVO> selectDailyWorkIssues(Set<Long> projectIds) {
        if (CollectionUtils.isEmpty(projectIds)) {
            return new ArrayList<>();
        }
        return issueMapper.selectDailyWorkIssues(projectIds);
    }

    private Map<Long, UserDTO> getUserMap(Set<Long> userIds) {
        Map<Long, UserDTO> userMap = new HashMap<>();
        if (!CollectionUtils.isEmpty(userIds)) {
            List<UserDTO> userList = baseFeignClient.listUsersByIds(userIds.toArray(new Long[0]), true).getBody();
            if (CollectionUtils.isEmpty(userList)) {
                return userMap;
            }
            userMap.putAll(userList.stream().collect(Collectors.toMap(UserDTO::getId, Function.identity())));
        }
        return userMap;
    }

    private MultiKeyMap getMultiKeyMap(List<IssueDailyWorkVO> issueList) {
        MultiKeyMap multiKeyMap = new MultiKeyMap();
        issueList.forEach(issue -> {
            Long projectId = issue.getProjectId();
            Long assigneeId = issue.getAssigneeId();
            List<Long> issueIds = (List<Long>) multiKeyMap.get(projectId, assigneeId);
            if (CollectionUtils.isEmpty(issueIds)) {
                issueIds = new ArrayList<>();
                multiKeyMap.put(projectId, assigneeId, issueIds);
            }
            issueIds.add(issue.getIssueId());
        });
        return multiKeyMap;
    }

    private void setChildIssues(List<IssueDailyWorkVO> list) {
        Map<Long, List<IssueDailyWorkVO>> childIssueMap = new HashMap<>();
        Set<Long> issueIds = list.stream().map(IssueDailyWorkVO::getIssueId).collect(Collectors.toSet());
        Iterator<IssueDailyWorkVO> iterator = list.listIterator();
        while(iterator.hasNext()) {
            IssueDailyWorkVO issue = iterator.next();
            Long parentId = !Objects.isNull(issue.getParentIssueId()) ? issue.getParentIssueId() : issue.getRelateIssueId();
            if (!Objects.isNull(parentId) && issueIds.contains(parentId)) {
                List<IssueDailyWorkVO> childIssues = childIssueMap.get(parentId);
                if (CollectionUtils.isEmpty(childIssues)) {
                    childIssues = new ArrayList<>();
                    childIssueMap.put(parentId, childIssues);
                }
                childIssues.add(issue);
                iterator.remove();
            }
        }
        list.forEach(v -> v.setChildIssues(childIssueMap.get(v.getIssueId())));
    }

    private Map<String, String> buildParamMap(List<IssueDailyWorkVO> issueList, ProjectMessageVO projectMessageVO, UserDTO user) {
        Map<String, String> result = new HashMap<>();
        result.put(PROJECT_NAME, projectMessageVO.getName());
        result.put(USER_NAME, user.getRealName());
        result.put(DELAY_COUNT, getDelayCount(issueList) + "");
        result.put(UNFINISHED_COUNT, issueList.size() + "");
        //统计未完成个数、逾期个数后，调整父子层级
        setChildIssues(issueList);
        result.put(HTML_TABLE, buildHtmlTable(issueList, projectMessageVO));
        return result;
    }

    private int getDelayCount(List<IssueDailyWorkVO> issueList) {
        int delayCount = 0;
        Date date = new Date();
        for (IssueDailyWorkVO issue : issueList) {
            if (!Objects.isNull(issue.getEstimatedEndTime()) && date.after(issue.getEstimatedEndTime())) {
                delayCount++;
            }
        }
        return delayCount;
    }

    private String buildHtmlTable(List<IssueDailyWorkVO> issueList, ProjectMessageVO projectMessageVO) {
        String prefix = getPrefix(projectMessageVO);
        StringBuilder builder = new StringBuilder();
        Date now = new Date();
        LocalDateTime localDateTime = now.toInstant().atZone(ZoneId.systemDefault()).toLocalDateTime();

        builder.append(TABLE_TAG).append(TABLE_HEAD);

        int index = 1;
        for (IssueDailyWorkVO issueDailyWorkVO : issueList) {
            if (Objects.isNull(issueDailyWorkVO)) {
                continue;
            }
            appendIssueTr(prefix, builder, issueDailyWorkVO, localDateTime, false, index++);
            if (!CollectionUtils.isEmpty(issueDailyWorkVO.getChildIssues())) {
                for (IssueDailyWorkVO chileIssue : issueDailyWorkVO.getChildIssues()) {
                    appendIssueTr(prefix, builder, chileIssue, localDateTime, true, index++);
                }
            }
        }
        builder.append(BACKSLASH_TABLE);
        return builder.toString();
    }

    private String getPrefix(ProjectMessageVO projectMessageVO) {
        Long projectId = projectMessageVO.getId();
        Long organizationId = projectMessageVO.getOrganizationId();
        String projectName = projectMessageVO.getName();
        return domainUrl +
                "/#/agile/work-list/issue?type=project&id=" +
                projectId +
                "&name=" +
                projectName +
                "&organizationId=" +
                organizationId +
                "&orgId=" +
                organizationId;
    }

    private void appendIssueTr(String prefix, StringBuilder builder, IssueDailyWorkVO issue, LocalDateTime localDateTime, boolean isChild, int index) {
        Long issueId = issue.getIssueId();
        String url =
                prefix
                        + "&paramName="
                        + issue.getIssueNum()
                        + "&paramIssueId="
                        + issueId
                        + "&paramOpenIssueId=" + issueId;
        if (index % 2 != 0) {
            builder.append(TR_TAG_SINGLE);
        } else {
            builder.append(TR_TAG_DOUBLE);
        }

        builder.append(TD_TAG).append(BACKSLASH_TD);
        if (isChild) {
            builder
                    .append(TD_TAG).append("<span style=\"padding-left: 25px; display: inline-block;\">")
                    .append(getIssueTypeName(issue))
                    .append("<a href=\"").append(url).append("\" target=\"_blank\">")
                    .append(issue.getSummary()).append("</a>")
                    .append(getDelayDays(localDateTime, issue.getEstimatedEndTime())).append(BACKSLASH_SPAN).append(BACKSLASH_TD);
        } else {
            builder
                    .append(TD_TAG)
                    .append(getIssueTypeName(issue))
                    .append("<a href=\"").append(url).append("\" target=\"_blank\">")
                    .append(issue.getSummary()).append("</a>")
                    .append(getDelayDays(localDateTime, issue.getEstimatedEndTime())).append(BACKSLASH_TD);
        }
        builder
                .append(TD_TAG).append(getIssueNum(issue)).append(BACKSLASH_TD)
                .append(TD_TAG).append(getPriority(issue)).append(BACKSLASH_TD)
                .append(TD_TAG).append(getStatus(issue)).append(BACKSLASH_TD)
                .append(BACKSLASH_TR);
    }

    private String getIssueTypeName(IssueDailyWorkVO issueDailyWorkVO) {
        String issueTypeName = issueDailyWorkVO.getTypeName();
        return "<span style=\"color: #0F1358;\">"+ issueTypeName + "：" + BACKSLASH_SPAN;
    }

    private String getDelayDays(LocalDateTime localDateTime, Date estimatedEndTime) {
        if (Objects.isNull(estimatedEndTime)) {
            return "";
        }
        LocalDateTime estimatedEndDate = estimatedEndTime.toInstant().atZone(ZoneId.systemDefault()).toLocalDateTime();
        long delayDays = estimatedEndDate.isBefore(localDateTime) ?
                Duration.between(estimatedEndDate, localDateTime).toDays() + 1 : 0;
        return delayDays > 0 ? "&nbsp;&nbsp;" + RED_SPAN + "逾期" + delayDays + "天" + BACKSLASH_SPAN : "";
    }

    private String getIssueNum(IssueDailyWorkVO issueDailyWorkVO) {
        String issueNum = issueDailyWorkVO.getIssueNum();
        return "<span style=\"color: #0F1358;\">"+ issueNum +BACKSLASH_SPAN;
    }

    private String getPriority(IssueDailyWorkVO issueDailyWorkVO) {
        String priorityColor = issueDailyWorkVO.getPriorityColour();
        String priorityName = issueDailyWorkVO.getPriorityName();
        return "<span style=\"padding: 2px 3px; font-size: 12px; height: 20px; border-radius: 2px; color: " + priorityColor + " ; background-color: " + priorityColor + "1F;\">"+ priorityName +BACKSLASH_SPAN;
    }

    private String getStatus(IssueDailyWorkVO issueDailyWorkVO) {
        String statusName = issueDailyWorkVO.getStatusName();
        String statusCode = issueDailyWorkVO.getStatusCode();
        String statusColor = STATUS_COLOR_MAP.get(statusCode);
        return "<span style=\"padding: 2px 3px; font-size: 12px; height: 20px; border-radius: 2px; color: #FFFFFF; background-color: " + statusColor +";\">"+ statusName +BACKSLASH_SPAN;
    }

    private MessageSender getMessageSender(Map<String, String> paramMap, UserDTO user, Long projectId) {
        MessageSender messageSender = delayTaskService.buildSender(0L, ISSUE_DAILY_WORK, paramMap, Collections.singletonList(user));
        Map<String, Object> additionalInformationMap = new HashMap<>();
        additionalInformationMap.put(MessageAdditionalType.PARAM_PROJECT_ID.getTypeName(), projectId);
        messageSender.setAdditionalInformation(additionalInformationMap);
        return messageSender;
    }
}
