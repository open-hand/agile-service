package io.choerodon.agile.infra.task;

import static org.slf4j.LoggerFactory.getLogger;

import java.text.SimpleDateFormat;
import java.time.Duration;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.keyvalue.MultiKey;
import org.apache.commons.collections4.map.MultiKeyMap;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.util.ObjectUtils;

import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.app.service.DelayTaskService;
import io.choerodon.agile.app.service.PriorityService;
import io.choerodon.agile.app.service.StatusService;
import io.choerodon.agile.infra.dto.UserDTO;
import io.choerodon.agile.infra.dto.business.IssueDTO;
import io.choerodon.agile.infra.enums.StatusNoticeUserType;
import io.choerodon.agile.infra.feign.operator.RemoteIamOperator;
import io.choerodon.agile.infra.mapper.FieldValueMapper;
import io.choerodon.agile.infra.mapper.IssueMapper;
import io.choerodon.agile.infra.mapper.StarBeaconMapper;
import io.choerodon.asgard.schedule.annotation.JobTask;
import io.choerodon.asgard.schedule.annotation.TimedTask;
import io.choerodon.asgard.schedule.enums.TriggerTypeEnum;
import io.choerodon.core.enums.MessageAdditionalType;

import org.hzero.boot.message.entity.MessageSender;
import org.hzero.core.base.BaseConstants;

/**
 * @author superlee
 * @since 2021-03-02
 */
@Component
public class IssueDelaySendMessageTask {

    private static final Logger LOGGER = getLogger(IssueDelaySendMessageTask.class);

    private static final String ISSUE_DELAY = "ISSUE_DELAY";
    private static final String PROJECT_NAME = "projectName";
    private static final String HTML_TABLE = "htmlTable";
    private static final String OUT_HTML_TABLE = "outHtmlTable";
    private static final String PROJECT_OWNER = "projectOwner";
    private static final String ASSIGNEE = "assignee";
    private static final String REPORTER = "reporter";
    private static final String MAIN_RESPONSIBLE = "mainResponsible";
    private static final String SPECIFIER = "specifier";
    private static final String STAR_USER = "starUser";
    private static final String DELAY_COUNT = "delayCount";

    private static final String BACKSLASH_TR = "</tr>";
    private static final String BACKSLASH_TD = "</td>";
    private static final String SPAN_TAG = "<span style=\"color: #0F1358;\">";
    private static final String BACKSLASH_SPAN = "</span>";
    private static final String TR_TAG_SINGLE = "<tr style = \"background-color: #FFFFFF; height: 40px; line-height: 25px;\">";
    private static final String TR_TAG_DOUBLE = "<tr style = \"background-color: #F5F5FC; height: 40px; line-height: 25px;\">";
    private static final String P_PROJECT_TABLE = "<p style=\"color: rgb(0, 0, 0); font-family: Ubuntu, Helvetica, Arial, sans-serif; " +
            "font-size: 14px; text-align: justify; margin-bottom: 0px; line-height: 24px;\">%s</p>";
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
            "<th style=\"word-break: break-all;color: #0F1358;border-width: 0 0 0 1px;font-size: 13px;line-height: 20px;margin: 0;overflow: visible;padding: 5px 5px;\" width=\"2%\"></th>" +
            "<th style=\"word-break: break-all;color: #0F1358;border-width: 0 0 0 1px;font-size: 13px;line-height: 20px;margin: 0;overflow: visible;padding: 5px 5px;\" width=\"10%\">概要</th>" +
            "<th style=\"word-break: break-all;color: #0F1358;border-width: 0 0 0 1px;font-size: 13px;line-height: 20px;margin: 0;overflow: visible;padding: 5px 5px;\" width=\"10%\">编号</th>" +
            "<th style=\"word-break: break-all;color: #0F1358;border-width: 0 0 0 1px;font-size: 13px;line-height: 20px;margin: 0;overflow: visible;padding: 5px 5px;\" width=\"10%\">状态</th>" +
            "<th style=\"word-break: break-all;color: #0F1358;border-width: 0 0 0 1px;font-size: 13px;line-height: 20px;margin: 0;overflow: visible;padding: 5px 5px;\" width=\"10%\">优先级</th>" +
            "<th style=\"word-break: break-all;color: #0F1358;border-width: 0 0 0 1px;font-size: 13px;line-height: 20px;margin: 0;overflow: visible;padding: 5px 5px;\" width=\"14%\">预计开始时间</th>" +
            "<th style=\"word-break: break-all;color: #0F1358;border-width: 0 0 0 1px;font-size: 13px;line-height: 20px;margin: 0;overflow: visible;padding: 5px 5px;\" width=\"14%\">预计结束时间</th>" +
            "<th style=\"word-break: break-all;color: #0F1358;border-width: 0 0 0 1px;font-size: 13px;line-height: 20px;margin: 0;overflow: visible;padding: 5px 5px;\" width=\"10%\">经办人</th>" +
            "<th style=\"word-break: break-all;color: #0F1358;border-width: 0 0 0 1px;font-size: 13px;line-height: 20px;margin: 0;overflow: visible;padding: 5px 5px;\" width=\"10%\">报告人</th>" +
            "<th style=\"word-break: break-all;color: #0F1358;border-width: 0 0 0 1px;font-size: 13px;line-height: 20px;margin: 0;overflow: visible;padding: 5px 5px;\" width=\"10%\">逾期天数</th>" +
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


    private final SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");

    @Autowired
    private RemoteIamOperator remoteIamOperator;
    @Autowired
    private DelayTaskService delayTaskService;
    @Autowired
    private IssueMapper issueMapper;
    @Autowired
    private PriorityService priorityService;
    @Autowired
    private StatusService statusService;
    @Autowired
    private StarBeaconMapper starBeaconMapper;
    @Autowired
    private FieldValueMapper fieldValueMapper;

    @Value("${services.domain.url}")
    private String domainUrl;

    @JobTask(
            maxRetryCount = 3,
            code = "issueDelaySendMessage",
            description = "问题延期，发送消息给指定人"
    )
    @TimedTask(
            name = "issueDelaySendMessage",
            description = "问题延期，发送消息给指定人",
            oneExecution = false,
            params = {},
            triggerType = TriggerTypeEnum.CRON_TRIGGER,
            cronExpression = "0 30 2 * * ? "
    )
    public void run(Map<String, Object> map) {
        LOGGER.info("===> 开始执行问题延期发送消息定时任务");
        Map<Long, ProjectMessageVO> projectMap = delayTaskService.listEnabledMsgProjects(ISSUE_DELAY);
        if (ObjectUtils.isEmpty(projectMap)) {
            LOGGER.info("===> 没有配置发送消息的项目，问题延期发送消息定时任务完成");
            return;
        }
        Set<Long> projectIds = projectMap.keySet();
        //key1 projectId, key2 userId, value Set<IssueDelayCarrierVO>
        MultiKeyMap<Long, Set<IssueDelayCarrierVO>> multiKeyMap = new MultiKeyMap<>();
        Map<Long, UserDTO> userMap = new HashMap<>();
        Map<Long, PriorityVO> priorityMap = new HashMap<>();
        Map<Long, StatusVO> statusMap = new HashMap<>();
        if (!projectIds.isEmpty()) {
            Date now = new Date();
            List<IssueDTO> issues = issueMapper.selectDelayIssues(projectIds, now);
            Map<Long, List<IssueDTO>> issueGroupByProject =
                    issues.stream().collect(Collectors.groupingBy(IssueDTO::getProjectId));
            Set<Long> userIds = new HashSet<>();
            Set<Long> projectIdForProjectOwner = new HashSet<>();
            LocalDateTime localDateTime = now.toInstant().atZone(ZoneId.systemDefault()).toLocalDateTime();
            processIssueToMultiKeyMap(
                    projectMap,
                    multiKeyMap,
                    localDateTime,
                    issueGroupByProject,
                    userIds,
                    projectIdForProjectOwner,
                    priorityMap,
                    statusMap);
            processProjectOwner(projectMap, multiKeyMap, issueGroupByProject, userIds, projectIdForProjectOwner, localDateTime);
            if (!userIds.isEmpty()) {
                userMap.putAll(
                        Objects.requireNonNull(remoteIamOperator.listUsersByIds(userIds.toArray(new Long[0]), true))
                                .stream()
                                .collect(Collectors.toMap(UserDTO::getId, Function.identity()))
                );
            }
        }
        List<MessageSender> messageSenders = new ArrayList<>();
        for (Map.Entry<MultiKey<? extends Long>, Set<IssueDelayCarrierVO>> entry : multiKeyMap.entrySet()) {
            final MultiKey<? extends Long> multiKey = entry.getKey();
            Long projectId = multiKey.getKey(0);
            Long userId = multiKey.getKey(1);
            Set<IssueDelayCarrierVO> set = entry.getValue();
            if (!ObjectUtils.isEmpty(set)) {
                List<IssueDelayCarrierVO> list = new ArrayList<>(set);
                list.sort(Comparator.comparing(IssueDelayCarrierVO::getIssueId));
                UserDTO user = userMap.get(userId);
                if (user == null) {
                    continue;
                }
                Map<String, String> paramMap = buildParamMap(list, projectMap.get(projectId), priorityMap, statusMap, userMap);
                MessageSender messageSender = delayTaskService.buildSender(BaseConstants.DEFAULT_TENANT_ID, ISSUE_DELAY, paramMap, Collections.singletonList(user));
                Map<String, Object> additionalInformationMap = new HashMap<>();
                additionalInformationMap.put(MessageAdditionalType.PARAM_PROJECT_ID.getTypeName(), projectId);
                messageSender.setAdditionalInformation(additionalInformationMap);
                messageSenders.add(messageSender);
            }
        }
        // 批量发送通知
        delayTaskService.batchSendMessage(messageSenders, 100);
        LOGGER.info("===> 问题延期发送消息定时任务完成");
    }

    private Map<String, String> buildParamMap(List<IssueDelayCarrierVO> list,
                                              ProjectMessageVO projectMessageVO,
                                              Map<Long, PriorityVO> priorityMap,
                                              Map<Long, StatusVO> statusMap,
                                              Map<Long, UserDTO> userMap) {
        Map<String, String> result = new HashMap<>();
        String projectName = projectMessageVO.getName();
        result.put(PROJECT_NAME, projectName);
        result.put(DELAY_COUNT, list.size() + StringUtils.EMPTY);
        result.put(HTML_TABLE, String.format(P_PROJECT_TABLE, buildHtmlTable(list, projectMessageVO, priorityMap, statusMap, userMap, false)));
        result.put(OUT_HTML_TABLE, String.format(P_PROJECT_TABLE, buildHtmlTable(list, projectMessageVO, priorityMap, statusMap, userMap, true)));
        return result;
    }

    private String buildHtmlTable(List<IssueDelayCarrierVO> list,
                                  ProjectMessageVO projectMessageVO,
                                  Map<Long, PriorityVO> priorityMap,
                                  Map<Long, StatusVO> statusMap,
                                  Map<Long, UserDTO> userMap, boolean isOut) {
        Long projectId = projectMessageVO.getId();
        Long organizationId = projectMessageVO.getOrganizationId();
        String projectName = projectMessageVO.getName();
        StringBuilder prefix = new StringBuilder();
        if (isOut) {
            prefix.append(domainUrl);
        }
        prefix.append("/#/agile/work-list/issue?type=project&id=")
                .append(projectId)
                .append("&name=")
                .append(projectName)
                .append("&organizationId=")
                .append(organizationId)
                .append("&orgId=")
                .append(organizationId);
        StringBuilder builder = new StringBuilder();
        builder
                .append(TABLE_TAG)
                .append(TABLE_HEAD);
        Iterator<IssueDelayCarrierVO> iterator = list.iterator();
        int index = 0;
        while (iterator.hasNext()) {
            index++;
            IssueDelayCarrierVO vo = iterator.next();
            Long issueId = vo.getIssueId();
            IssueDTO dto = vo.getIssueDTO();
            if (dto == null) {
                continue;
            }
            String url =
                    prefix
                            + "&paramName="
                            + dto.getIssueNum()
                            + "&paramIssueId="
                            + issueId
                            + "&paramOpenIssueId=" + issueId;
            PriorityVO priorityVO = priorityMap.get(dto.getPriorityId());
            StatusVO statusVO = statusMap.get(dto.getStatusId());
            String startDate = Optional.ofNullable(dto.getEstimatedStartTime()).map(sdf::format).orElse(StringUtils.EMPTY);
            String endDate = Optional.ofNullable(dto.getEstimatedEndTime()).map(sdf::format).orElse(StringUtils.EMPTY);
            String assignee = StringUtils.EMPTY;
            if (dto.getAssigneeId() != null) {
                UserDTO user = userMap.get(dto.getAssigneeId());
                if (user != null) {
                    assignee = getNameByLdap(user);
                }
            }
            String reporter = StringUtils.EMPTY;
            if (dto.getReporterId() != null) {
                UserDTO user = userMap.get(dto.getReporterId());
                if (user != null) {
                    reporter = getNameByLdap(user);
                }
            }
            if (index % 2 != 0) {
                builder.append(TR_TAG_SINGLE).append(TD_TAG).append(BACKSLASH_TD);
            } else {
                builder.append(TR_TAG_DOUBLE).append(TD_TAG).append(BACKSLASH_TD);
            }
            builder
                    .append(TD_TAG).append("<a href=\"").append(url).append("\" target=\"_blank\" style=\" text-decoration: none; color: #5365EA\">").append(dto.getSummary()).append("</a>").append(BACKSLASH_TD)
                    .append(TD_TAG).append(SPAN_TAG).append(dto.getIssueNum()).append(BACKSLASH_SPAN).append(BACKSLASH_TD)
                    .append(TD_TAG).append(getStatus(statusVO)).append(BACKSLASH_TD)
                    .append(TD_TAG).append(getPriority(priorityVO)).append(BACKSLASH_TD)
                    .append(TD_TAG).append(SPAN_TAG).append(startDate).append(BACKSLASH_SPAN).append(BACKSLASH_TD)
                    .append(TD_TAG).append(SPAN_TAG).append(endDate).append(BACKSLASH_SPAN).append(BACKSLASH_TD)
                    .append(TD_TAG).append(SPAN_TAG).append(assignee).append(BACKSLASH_SPAN).append(BACKSLASH_TD)
                    .append(TD_TAG).append(SPAN_TAG).append(reporter).append(BACKSLASH_SPAN).append(BACKSLASH_TD)
                    .append(TD_TAG).append(SPAN_TAG).append(vo.getDelayDay()).append(BACKSLASH_SPAN).append(BACKSLASH_TD)
                    .append(BACKSLASH_TR);
        }
        builder.append("</table>");
        return builder.toString();
    }

    private String getPriority(PriorityVO priorityVO) {
        if (!Objects.isNull(priorityVO)) {
            String priorityColor = priorityVO.getColour();
            String priorityName = priorityVO.getName();
            return "<span style=\"padding: 2px 3px; font-size: 12px; height: 20px; border-radius: 2px; color: " + priorityColor + " ; background-color: " + priorityColor + "1F;\">"+ priorityName +BACKSLASH_SPAN;
        }
        return StringUtils.EMPTY;
    }

    private String getStatus(StatusVO statusVO) {
        if (!Objects.isNull(statusVO)) {
            String statusName = statusVO.getName();
            String statusType = statusVO.getType();
            String statusColor = STATUS_COLOR_MAP.get(statusType);
            return "<span style=\"padding: 2px 3px; font-size: 12px; height: 20px; border-radius: 2px; color: #FFFFFF; background-color: " + statusColor +";\">"+ statusName +BACKSLASH_SPAN;
        }
        return StringUtils.EMPTY;
    }

    private String getNameByLdap(UserDTO userDTO) {
        if (Boolean.TRUE.equals(userDTO.getLdap())) {
            return userDTO.getRealName() + "（" + userDTO.getLoginName() + "）";
        } else {
            return userDTO.getRealName() + "（" + userDTO.getEmail() + "）";
        }
    }

    private void processProjectOwner(Map<Long, ProjectMessageVO> projectMap, MultiKeyMap<Long, Set<IssueDelayCarrierVO>> multiKeyMap, Map<Long, List<IssueDTO>> issueGroupByProject, Set<Long> userIds, Set<Long> projectIdForProjectOwner, LocalDateTime localDateTime) {
        if (!projectIdForProjectOwner.isEmpty()) {
            List<ProjectWithUserVO> projectWithUserList =
                    remoteIamOperator.listProjectOwnerByIds(projectIdForProjectOwner);
            if(CollectionUtils.isEmpty(projectWithUserList)) {
                return;
            }
            for (ProjectWithUserVO x : projectWithUserList) {
                Long projectId = x.getProjectId();
                userIds.addAll(x.getUserIds());
                for (Long y : x.getUserIds()) {
                    List<IssueDTO> issueList = issueGroupByProject.get(projectId);
                    if (CollectionUtils.isNotEmpty(issueList)) {
                        for (IssueDTO z : issueList) {
                            Long organizationId = projectMap.get(projectId).getOrganizationId();
                            LocalDateTime estimatedEndDate = z.getEstimatedEndTime().toInstant().atZone(ZoneId.systemDefault()).toLocalDateTime();

                            long delayDay = estimatedEndDate.isBefore(localDateTime) ?
                                    Duration.between(estimatedEndDate, localDateTime).toDays() + 1 : 0;
                            addValueToMultiKeyMap(
                                    multiKeyMap,
                                    projectId,
                                    y,
                                    new IssueDelayCarrierVO(z, z.getIssueId(), delayDay, organizationId));
                        }
                    }
                }
            }
        }
    }

    private void processIssueToMultiKeyMap(Map<Long, ProjectMessageVO> projectMap,
                                           MultiKeyMap<Long, Set<IssueDelayCarrierVO>> multiKeyMap,
                                           LocalDateTime localDateTime,
                                           Map<Long, List<IssueDTO>> issueGroupByProject,
                                           Set<Long> userIds,
                                           Set<Long> projectIdForProjectOwner,
                                           Map<Long, PriorityVO> priorityVOMap,
                                           Map<Long, StatusVO> statusVOMap) {
        Map<Long, Map<Long, PriorityVO>> priorityMap = new HashMap<>();
        Map<Long, Map<Long, StatusVO>> statusMap = new HashMap<>();
        for (Map.Entry<Long, List<IssueDTO>> entry : issueGroupByProject.entrySet()) {
            Long key = entry.getKey();
            List<IssueDTO> issueList = entry.getValue();
            ProjectMessageVO projectMessageVO = projectMap.get(key);
            Set<String> receiverTypes = projectMessageVO.getReceiverTypes();
            Long organizationId = projectMessageVO.getOrganizationId();
            queryOrganizationPriorityMap(organizationId, priorityMap);
            queryOrganizationStatusMap(organizationId, statusMap);
            for (IssueDTO x : issueList) {
                Date estimatedEndTime = x.getEstimatedEndTime();
                LocalDateTime estimatedEndDate = estimatedEndTime.toInstant().atZone(ZoneId.systemDefault()).toLocalDateTime();
                long delayDay = estimatedEndDate.isBefore(localDateTime) ?
                        Duration.between(estimatedEndDate, localDateTime).toDays() + 1 : 0;
                Map<String, Set<Long>> userTypeMap = new HashMap<>();
                if (receiverTypes != null) {
                    addSystemUserType(ASSIGNEE, x.getAssigneeId(), userTypeMap, userIds);
                    addSystemUserType(REPORTER, x.getReporterId(), userTypeMap, userIds);
                    addSystemUserType(MAIN_RESPONSIBLE, x.getMainResponsibleId(), userTypeMap, userIds);
                    userTypeMap.put(SPECIFIER, projectMessageVO.getUserIds());
                    if (receiverTypes.contains(PROJECT_OWNER)) {
                        projectIdForProjectOwner.add(key);
                    }
                    //问题逾期通知增加关注人
                    userTypeMap.put(STAR_USER, new HashSet<>(starBeaconMapper.selectUsersByInstanceId(key, x.getIssueId())));
                    addToMultiKeyMap(x, projectMessageVO, delayDay, multiKeyMap, userIds, userTypeMap);
                    //问题逾期通知增加自定义人员字段选项
                    addCustomUserType(organizationId, x, delayDay, receiverTypes, multiKeyMap, userIds);
                }
            }
        }
        priorityMap.values().forEach(priorityVOMap::putAll);
        statusMap.values().forEach(statusVOMap::putAll);
    }

    private void addCustomUserType(Long organizationId,
                                   IssueDTO issueDTO,
                                   long delayDay,
                                   Set<String> receiverTypes,
                                   MultiKeyMap<Long, Set<IssueDelayCarrierVO>> multiKeyMap,
                                   Set<Long> userIds) {
        List<String> customUserTypes = new ArrayList<>(receiverTypes);
        customUserTypes.removeAll(Arrays.asList(StatusNoticeUserType.BASE_USER_TYPE_LIST));
        if (CollectionUtils.isNotEmpty(customUserTypes)) {
            List<Long> customFieldUserIds = fieldValueMapper.selectUserIdByField(issueDTO.getProjectId(), customUserTypes, issueDTO.getIssueId());
            if (CollectionUtils.isNotEmpty(customFieldUserIds)) {
                for (Long userId : customFieldUserIds) {
                    userIds.add(userId);
                    addValueToMultiKeyMap(multiKeyMap,
                            issueDTO.getProjectId(),
                            userId,
                            new IssueDelayCarrierVO(issueDTO, issueDTO.getIssueId(), delayDay, organizationId));
                }
            }
        }
    }

    private void addSystemUserType(String userType, Long userId, Map<String, Set<Long>> userTypeMap, Set<Long> userIds) {
        if (userId != null && !Objects.equals(0L, userId)) {
            userIds.add(userId);
            userTypeMap.put(userType, Collections.singleton(userId));
        }
    }

    private void addToMultiKeyMap(IssueDTO issueDTO,
                                  ProjectMessageVO projectMessageVO,
                                  long delayDay,
                                  MultiKeyMap<Long, Set<IssueDelayCarrierVO>> multiKeyMap,
                                  Set<Long> userIds,
                                  Map<String,Set<Long>> userTypeMap) {
        Set<String> receiverTypes = projectMessageVO.getReceiverTypes();
        Long organizationId = projectMessageVO.getOrganizationId();

        for (Map.Entry<String, Set<Long>> entry : userTypeMap.entrySet()) {
            String userType = entry.getKey();
            Set<Long> userIdList = entry.getValue();
            if (receiverTypes.contains(userType) && CollectionUtils.isNotEmpty(userIdList)) {
                for (Long userId : userIdList) {
                    userIds.add(userId);
                    addValueToMultiKeyMap(multiKeyMap,
                            issueDTO.getProjectId(),
                            userId,
                            new IssueDelayCarrierVO(issueDTO, issueDTO.getIssueId(), delayDay, organizationId));
                }
            }
        }
    }

    private void queryOrganizationStatusMap(Long organizationId,
                                            Map<Long, Map<Long, StatusVO>> statusMap) {
        if (ObjectUtils.isEmpty(statusMap.get(organizationId))) {
            Map<Long, StatusVO> orgStatusMap = new HashMap<>(statusService.queryAllStatusMap(organizationId));
            statusMap.put(organizationId, orgStatusMap);
        }
    }

    private void queryOrganizationPriorityMap(Long organizationId,
                                              Map<Long, Map<Long, PriorityVO>> priorityMap) {
        if (ObjectUtils.isEmpty(priorityMap.get(organizationId))) {
            Map<Long, PriorityVO> orgPriorityMap = new HashMap<>(priorityService.queryByOrganizationId(organizationId));
            priorityMap.put(organizationId, orgPriorityMap);
        }
    }

    private void addValueToMultiKeyMap(MultiKeyMap<Long, Set<IssueDelayCarrierVO>> multiKeyMap,
                                       Long projectId,
                                       Long userId,
                                       IssueDelayCarrierVO issueDelayCarrierVO) {
        Set<IssueDelayCarrierVO> set = multiKeyMap.get(projectId, userId);
        if (set == null) {
            set = new HashSet<>();
            multiKeyMap.put(projectId, userId, set);
        }
        set.add(issueDelayCarrierVO);
    }


}
