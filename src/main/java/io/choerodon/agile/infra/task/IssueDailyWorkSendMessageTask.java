package io.choerodon.agile.infra.task;

import static org.slf4j.LoggerFactory.getLogger;

import java.time.Duration;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

import com.yqcloud.core.oauth.ZKnowDetailsHelper;
import org.apache.commons.collections4.keyvalue.MultiKey;
import org.apache.commons.collections4.map.MultiKeyMap;
import org.slf4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;

import io.choerodon.agile.api.vo.IssueDailyWorkVO;
import io.choerodon.agile.api.vo.ProjectMessageVO;
import io.choerodon.agile.app.service.DelayTaskService;
import io.choerodon.agile.infra.dto.UserDTO;
import io.choerodon.agile.infra.feign.operator.RemoteIamOperator;
import io.choerodon.agile.infra.feign.vo.OrganizationInfoVO;
import io.choerodon.agile.infra.mapper.IssueMapper;
import io.choerodon.asgard.schedule.annotation.JobTask;
import io.choerodon.asgard.schedule.annotation.TimedTask;
import io.choerodon.asgard.schedule.enums.TriggerTypeEnum;
import io.choerodon.core.enums.MessageAdditionalType;

import org.hzero.boot.message.entity.MessageSender;


/**
 * @author huaxin.deng@hand-china.com 2021-08-02 10:01:14
 */
@Component
public class IssueDailyWorkSendMessageTask {

    private static final Logger LOGGER = getLogger(IssueDailyWorkSendMessageTask.class);

    private static final String ISSUE_DAILY_WORK = "ISSUE_DAILY_WORK";
    private static final String USER_NAME = "userName";
    private static final String ORGANIZATION_NAME = "organizationName";
    private static final String HTML_TABLE = "htmlTable";
    private static final String PM_HTML_TABLE = "pmHtmlTable";
    private static final String EMAIL_HTML_TABLE = "emailHtmlTable";
    private static final String BACKSLASH_TR = "</tr>";
    private static final String BACKSLASH_TD = "</td>";
    private static final String BACKSLASH_SPAN = "</span>";
    private static final String BACKSLASH_TABLE = "</table>";
    private static final String TR_TAG_SINGLE = "<tr style = \"background-color: #FFFFFF; height: 40px; line-height: 25px;\">";
    private static final String TR_TAG_DOUBLE = "<tr style = \"background-color: #F5F5FC; height: 40px; line-height: 25px;\">";
    private static final String P_PROJECT_STATISTICS = "<p style=\"color: #0F1358;font-family: PingFangSC-Regular;" +
            "font-size: 14px;text-align: justify;margin-bottom: 0px;margin-top: 0px; line-height: 24px;\">" +
            "<span style=\"display: inline-block; margin-right: 6px;width: 3px; height: 15px; background: #798DFB; border-radius: 2px;" +
            "vertical-align: middle;margin-bottom: 2px;\"></span>" +
            "%s：%s个问题未完成，其中%s个问题已逾期。</p>";
    private static final String P_PROJECT_TABLE = "<p style=\"color: rgb(0, 0, 0); font-family: Ubuntu, Helvetica, Arial, sans-serif; " +
            "font-size: 14px; text-align: justify; margin-bottom: 0px; line-height: 24px;\">%s</p>";
    private static final String RED_SPAN = "<span style=\"" +
            "color: #F5222D; " +
            "font-size: 12px; " +
            "font-family: PingFangSC-Regular; " +
            "border: 1px solid red; " +
            "border-radius: 2px; " +
            "padding: 1px 3px; " +
            "white-space:nowrap;\">";
    private static final String STYLE_ARROW_DROP_DOWN = "<span>" +
            "<style type=\"text/css\">" +
            "   .arrow_drop_down {" +
            "       width: 0px;" +
            "       height: 0px;" +
            "       border-top: 4px solid black;" +
            "       border-left: 4px solid transparent;" +
            "       border-right: 4px solid transparent;" +
            "       }" +
            "     </style>";
    private static final String DIV_ARROW_DROP_DOWN = "<div style=\"" +
            "margin-top: 10px !important;" +
            "margin-left: 40% !important;\"" +
            "class=\"arrow_drop_down\">" +
            "</div>";
    private static final String TABLE_TAG = "<table style=\"" +
            "width: 100%;" +
            "font-size: 10px;" +
            "table-layout: fixed;" +
            "border-collapse: collapse;" +
            "border-spacing: 0;" +
            "border: 1px solid #D9E6F2; " +
            "margin-top: 16px;" +
            "margin-bottom: 30px;" +
            "empty-cells: show;\">" +
            STYLE_ARROW_DROP_DOWN;
    private static final String TABLE_HEAD = "<thead style=\"background-color: #F3F6FE;color: #000;text-align: left;vertical-align: bottom;\">" +
            "<tr style = \"height: 30px;\">" +
            "<th style=\"word-break: break-all;color: #0F1358;border-width: 0 0 0 1px;font-size: 13px;line-height: 20px;margin: 0;overflow: visible;padding: 5px 5px;\" width=\"3%\"></th>" +
            "<th style=\"word-break: break-all;color: #0F1358;border-width: 0 0 0 1px;font-size: 13px;line-height: 20px;margin: 0;overflow: visible;padding: 5px 5px;\" width=\"54%\">概要</th>" +
            "<th style=\"word-break: break-all;color: #0F1358;border-width: 0 0 0 1px;font-size: 13px;line-height: 20px;margin: 0;overflow: visible;padding: 5px 5px;\" width=\"15%\">编号</th>" +
            "<th style=\"word-break: break-all;color: #0F1358;border-width: 0 0 0 1px;font-size: 13px;line-height: 20px;margin: 0;overflow: visible;padding: 5px 5px;\" width=\"13%\">优先级</th>" +
            "<th style=\"word-break: break-all;color: #0F1358;border-width: 0 0 0 1px;font-size: 13px;line-height: 20px;margin: 0;overflow: visible;padding: 5px 5px;\" width=\"15%\">状态</th>" +
            BACKSLASH_TR +
            "</thead>";
    private static final String TD_TAG = "<td style=\"" +
            "word-break: break-all;" +
            "padding-top: 8px;" +
            "padding-bottom: 8px;" +
            "padding-left: 5px;" +
            "border-width: 0 0 0 1px;" +
            "font-size: 13px;" +
            "margin: 0;" +
            "overflow: visible;\">";
    private static final String TD_ARROW_DROP_DOWN = "<td style=\"" +
            "word-break: break-all;" +
            "padding-top: 8px;" +
            "padding-right: 3px;" +
            "vertical-align: top;" +
            "border-width: 0 0 0 1px;" +
            "font-size: 13px;" +
            "margin: 0;" +
            "overflow: visible;\">";

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
    private RemoteIamOperator remoteIamOperator;
    @Autowired
    private DelayTaskService delayTaskService;
    @Autowired
    private IssueMapper issueMapper;

    @Value("${services.domain.url}")
    private String domainUrl;

    @JobTask(productSource = ZKnowDetailsHelper.VALUE_CHOERODON,
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
        // 批量发送通知
        delayTaskService.batchSendMessage(messageSenders, 100);
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
        Map<Long, List<IssueDailyWorkVO>> userIssueMap = issueList.stream()
                .collect(Collectors.groupingBy(IssueDailyWorkVO::getAssigneeId));

        //key1 userId, key2 projectId, value issueIds保持排序
        final MultiKeyMap<Long, List<Long>> userProjectIssueIdsMap = getUserProjectIssueIdsMap(issueList);
        //key1 userId, key2 organizationId, value projectIds
        final MultiKeyMap<Long, List<Long>> userOrgProjectIdsMap = getUserOrgProjectIdsMap(userIssueMap, projectMap);
        for (Map.Entry<MultiKey<? extends Long>, List<Long>> entry : userOrgProjectIdsMap.entrySet()) {
            final MultiKey<? extends Long> multiKey = entry.getKey();
            Long userId = multiKey.getKey(0);
            Long organizationId = multiKey.getKey(1);

            UserDTO user = userMap.get(userId);
            if (ObjectUtils.isEmpty(user)) {
                continue;
            }

            List<Long> userOrgProjectIds = entry.getValue();
            //从小到大排序
            if (CollectionUtils.isEmpty(userOrgProjectIds)) {
                continue;
            }
            Collections.sort(userOrgProjectIds);
            Map<String, String> paramMap = buildParamMap(organizationId, userOrgProjectIds, userProjectIssueIdsMap, issueMap, projectMap, user);

            //获取当前用户下在当前组织下需要发送通知的项目
            List<Long> senderProjectIds = getSenderProjectIds(projectMap, userOrgProjectIds);
            for (Long senderProjectId : senderProjectIds) {
                messageSenders.add(getMessageSender(paramMap, user, senderProjectId));
            }
        }
    }

    private List<Long> getSenderProjectIds(Map<Long, ProjectMessageVO> projectMap, List<Long> userOrgProjectIds) {
        List<ProjectMessageVO> filterProjects = projectMap.values().stream().filter(project -> userOrgProjectIds.contains(project.getId())).collect(Collectors.toList());
        List<Long> pmEnableProjectIds = filterProjects.stream().filter(project -> Boolean.TRUE.equals(project.getPmEnable())).map(ProjectMessageVO::getId).collect(Collectors.toList());
        List<Long> emailEnableProjectIds = filterProjects.stream().filter(project -> Boolean.TRUE.equals(project.getEmailEnable())).map(ProjectMessageVO::getId).collect(Collectors.toList());

        //取当前用户组织下需要发送站内信和邮件的交集项目
        List<Long> senderProjectIds = new ArrayList<>(pmEnableProjectIds);
        senderProjectIds.retainAll(emailEnableProjectIds);
        //若没有交集项目，则邮件和站内信分开发送
        if (CollectionUtils.isEmpty(senderProjectIds)) {
            if (!CollectionUtils.isEmpty(pmEnableProjectIds)) {
                senderProjectIds.add(pmEnableProjectIds.get(0));
            }
            if (!CollectionUtils.isEmpty(emailEnableProjectIds)) {
                senderProjectIds.add(emailEnableProjectIds.get(0));
            }
        }else {
            //若有交集项目，只发送一次
            Long senderProjectId = senderProjectIds.get(0);
            senderProjectIds.clear();
            senderProjectIds.add(senderProjectId);
        }
        return senderProjectIds;
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
            List<UserDTO> userList = remoteIamOperator.listUsersByIds(userIds.toArray(new Long[0]), true);
            if (CollectionUtils.isEmpty(userList)) {
                return userMap;
            }
            userMap.putAll(userList.stream().collect(Collectors.toMap(UserDTO::getId, Function.identity())));
        }
        return userMap;
    }

    private MultiKeyMap<Long, List<Long>> getUserOrgProjectIdsMap(Map<Long, List<IssueDailyWorkVO>> userIssueMap, Map<Long, ProjectMessageVO> projectMap) {
        MultiKeyMap<Long, List<Long>> multiKeyMap = new MultiKeyMap<>();
        //获取用户待办问题的所有项目
        for (Map.Entry<Long, List<IssueDailyWorkVO>> entry : userIssueMap.entrySet()) {
            Long userId = entry.getKey();
            List<IssueDailyWorkVO> issues = entry.getValue();
            Set<Long> userProjectIds = issues.stream().map(IssueDailyWorkVO::getProjectId).collect(Collectors.toSet());
            //将用户待办问题的所有项目按组织进行划分
            for (Long projectId : userProjectIds) {
                Long organizationId = projectMap.get(projectId).getOrganizationId();
                List<Long> userOrgProjectIds = multiKeyMap.get(userId, organizationId);
                if (CollectionUtils.isEmpty(userOrgProjectIds)) {
                    userOrgProjectIds = new ArrayList<>();
                    multiKeyMap.put(userId, organizationId, userOrgProjectIds);
                }
                userOrgProjectIds.add(projectId);
            }
        }
        return multiKeyMap;
    }

    private MultiKeyMap<Long, List<Long>> getUserProjectIssueIdsMap(List<IssueDailyWorkVO> issueList) {
        MultiKeyMap<Long, List<Long>> multiKeyMap = new MultiKeyMap<>();
        //将用户的待办问题按项目划分
        for (IssueDailyWorkVO issue : issueList) {
            Long projectId = issue.getProjectId();
            Long assigneeId = issue.getAssigneeId();
            List<Long> issueIds = multiKeyMap.get(assigneeId, projectId);
            if (CollectionUtils.isEmpty(issueIds)) {
                issueIds = new ArrayList<>();
                multiKeyMap.put(assigneeId, projectId, issueIds);
            }
            issueIds.add(issue.getIssueId());
        }
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

    private Map<String, String> buildParamMap(Long organizationId,
                                              List<Long> userOrgProjectIds,
                                              MultiKeyMap<Long, List<Long>> userProjectIssueIdsMap,
                                              Map<Long, IssueDailyWorkVO> issueMap,
                                              Map<Long, ProjectMessageVO> projectMap,
                                              UserDTO user) {
        OrganizationInfoVO organization = remoteIamOperator.query(organizationId);
        if(organization == null) {
            return Collections.emptyMap();
        }
        Map<String, String> result = new HashMap<>();
        result.put(USER_NAME, user.getRealName());
        result.put(ORGANIZATION_NAME, organization.getTenantName());
        Map<Long, String> projectTableMap = buildProjectHtmlTableMap(userOrgProjectIds, userProjectIssueIdsMap, issueMap, projectMap, user);

        //同一项目下每日工作提醒的邮件、站内信通知设置不一定全部开启，故分开拼接两种通知内容
        Map<String, StringBuilder> htmlTableMap = new HashMap<>();
        getHtmlTable(userOrgProjectIds, projectMap, projectTableMap, htmlTableMap);
        //邮件、站内信外的通知内容
        result.put(HTML_TABLE, htmlTableMap.get(HTML_TABLE).toString());
        //站内信通知内容
        result.put(PM_HTML_TABLE, htmlTableMap.get(PM_HTML_TABLE).toString());
        //邮件通知内容
        result.put(EMAIL_HTML_TABLE, htmlTableMap.get(EMAIL_HTML_TABLE).toString());
        return result;
    }

    private void getHtmlTable(List<Long> userOrgProjectIds,
                              Map<Long, ProjectMessageVO> projectMap,
                              Map<Long, String> projectTableMap,
                              Map<String, StringBuilder> htmlTableMap) {
        StringBuilder htmlTable = new StringBuilder();
        StringBuilder pmHtmlTable = new StringBuilder();
        StringBuilder emailHtmlTable = new StringBuilder();

        userOrgProjectIds.forEach(projectId -> {
            ProjectMessageVO projectMessageVO = projectMap.get(projectId);
            if (!Objects.isNull(projectMessageVO.getPmEnable()) && Boolean.TRUE.equals(projectMessageVO.getPmEnable())) {
                pmHtmlTable.append(projectTableMap.get(projectId));
            }
            if (!Objects.isNull(projectMessageVO.getEmailEnable()) && Boolean.TRUE.equals(projectMessageVO.getEmailEnable())) {
                emailHtmlTable.append(projectTableMap.get(projectId));
            }
            htmlTable.append(projectTableMap.get(projectId));
        });
        htmlTableMap.put(HTML_TABLE, htmlTable);
        htmlTableMap.put(PM_HTML_TABLE, pmHtmlTable);
        htmlTableMap.put(EMAIL_HTML_TABLE, emailHtmlTable);
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

    private Map<Long, String> buildProjectHtmlTableMap(List<Long> userOrgProjectIds,
                                                       MultiKeyMap<Long, List<Long>> userProjectIssueIdsMap,
                                                       Map<Long, IssueDailyWorkVO> issueMap,
                                                       Map<Long, ProjectMessageVO> projectMap,
                                                       UserDTO user) {
        Map<Long, String> projectTableMap = new HashMap<>();
        for (Long projectId : userOrgProjectIds) {
            List<Long> issueIds = userProjectIssueIdsMap.get(user.getId(), projectId);
            List<IssueDailyWorkVO> issueList = new ArrayList<>();
            issueIds.forEach(v -> issueList.add(issueMap.get(v)));
            ProjectMessageVO projectMessageVO = projectMap.get(projectId);
            StringBuilder projectTable = new StringBuilder();
            if (!CollectionUtils.isEmpty(issueList)) {
                //项目待办问题统计
                projectTable.append(String.format(P_PROJECT_STATISTICS,
                        projectMessageVO.getName(),
                        issueList.size(),
                        getDelayCount(issueList)
                        ));
                //统计未完成个数、逾期个数后，调整父子层级
                setChildIssues(issueList);
                //项目待办问题列表
                projectTable.append(String.format(P_PROJECT_TABLE,
                        buildProjectHtmlTable(issueList, projectMessageVO)));
                projectTableMap.put(projectId, projectTable.toString());
            }
        }
        return projectTableMap;
    }

    private String buildProjectHtmlTable(List<IssueDailyWorkVO> issueList, ProjectMessageVO projectMessageVO) {
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
        if (!isChild && !CollectionUtils.isEmpty(issue.getChildIssues())) {
            builder.append(TD_ARROW_DROP_DOWN).append(DIV_ARROW_DROP_DOWN).append(BACKSLASH_TD);
        }else {
            builder.append(TD_TAG).append(BACKSLASH_TD);
        }
        if (isChild) {
            builder
                    .append(TD_TAG).append("<span style=\"padding-left: 25px; display: inline-block;\">")
                    .append(getIssueTypeName(issue))
                    .append("<a href=\"").append(url).append("\" target=\"_blank\" style=\" text-decoration: none; color: #5365EA\">")
                    .append(issue.getSummary()).append("</a>")
                    .append(getDelayDays(localDateTime, issue.getEstimatedEndTime())).append(BACKSLASH_SPAN).append(BACKSLASH_TD);
        } else {
            builder
                    .append(TD_TAG)
                    .append(getIssueTypeName(issue))
                    .append("<a href=\"").append(url).append("\" target=\"_blank\" style=\" text-decoration: none; color: #5365EA\">")
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
