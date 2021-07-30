package io.choerodon.agile.infra.task;

import io.choerodon.agile.api.vo.IssueDelayCarrierVO;
import io.choerodon.agile.api.vo.ProjectMessageVO;
import io.choerodon.agile.api.vo.ProjectWithUserVO;
import io.choerodon.agile.app.service.DelayTaskService;
import io.choerodon.agile.app.service.PriorityService;
import io.choerodon.agile.app.service.StatusService;
import io.choerodon.agile.infra.dto.UserDTO;
import io.choerodon.agile.infra.dto.business.IssueDTO;
import io.choerodon.agile.infra.enums.StatusNoticeUserType;
import io.choerodon.agile.infra.feign.BaseFeignClient;
import io.choerodon.agile.infra.feign.NotifyFeignClient;
import io.choerodon.agile.infra.mapper.FieldValueMapper;
import io.choerodon.agile.infra.mapper.IssueMapper;
import io.choerodon.agile.infra.mapper.StarBeaconMapper;
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

import java.text.SimpleDateFormat;
import java.time.*;
import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

import static org.slf4j.LoggerFactory.getLogger;

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
    private static final String SPECIFIER = "specifier";
    private static final String STAR_USER = "starUser";
    private static final String DELAY_COUNT = "delayCount";

    private static final String BACKSLASH_TR = "</tr>";
    private static final String BACKSLASH_TD = "</td>";
    private static final String TD = "<td>";
    private static final String TR = "<tr>";

    private SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");

    @Autowired
    private NotifyFeignClient notifyFeignClient;
    @Autowired
    private BaseFeignClient baseFeignClient;
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
        MultiKeyMap multiKeyMap = new MultiKeyMap();
        Map<Long, UserDTO> userMap = new HashMap<>();
        Map<Long, String> priorityNameMap = new HashMap<>();
        Map<Long, String> statusNameMap = new HashMap<>();
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
                    priorityNameMap,
                    statusNameMap);
            processProjectOwner(projectMap, multiKeyMap, issueGroupByProject, userIds, projectIdForProjectOwner, localDateTime);
            if (!userIds.isEmpty()) {
                userMap.putAll(
                        baseFeignClient.listUsersByIds(userIds.toArray(new Long[userIds.size()]), true)
                                .getBody()
                                .stream()
                                .collect(Collectors.toMap(UserDTO::getId, Function.identity()))
                );
            }
        }
        List<MessageSender> messageSenders = new ArrayList<>();
        MapIterator mapIterator = multiKeyMap.mapIterator();
        while (mapIterator.hasNext()) {
            MultiKey multiKey = (MultiKey) mapIterator.next();
            Long projectId = (Long) multiKey.getKey(0);
            Long userId = (Long) multiKey.getKey(1);
            Set<IssueDelayCarrierVO> set = (Set<IssueDelayCarrierVO>) mapIterator.getValue();
            if (!ObjectUtils.isEmpty(set)) {
                List<IssueDelayCarrierVO> list = new ArrayList<>(set);
                list.sort(Comparator.comparing(IssueDelayCarrierVO::getIssueId));
                UserDTO user = userMap.get(userId);
                if (user == null) {
                    continue;
                }
                Map<String, String> paramMap = buildParamMap(list, projectMap.get(projectId), priorityNameMap, statusNameMap, userMap);
                MessageSender messageSender = delayTaskService.buildSender(0L, ISSUE_DELAY, paramMap, Arrays.asList(user));
                Map<String, Object> additionalInformationMap = new HashMap<>();
                additionalInformationMap.put(MessageAdditionalType.PARAM_PROJECT_ID.getTypeName(), projectId);
                messageSender.setAdditionalInformation(additionalInformationMap);
                messageSenders.add(messageSender);
            }
        }
        if (!messageSenders.isEmpty()) {
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
        LOGGER.info("===> 问题延期发送消息定时任务完成");
    }

    private Map<String, String> buildParamMap(List<IssueDelayCarrierVO> list,
                                              ProjectMessageVO projectMessageVO,
                                              Map<Long, String> priorityNameMap,
                                              Map<Long, String> statusNameMap,
                                              Map<Long, UserDTO> userMap) {
        Map<String, String> result = new HashMap<>();
        String projectName = projectMessageVO.getName();
        result.put(PROJECT_NAME, projectName);
        result.put(DELAY_COUNT, list.size() + "");
        result.put(HTML_TABLE, buildHtmlTable(list, projectMessageVO, priorityNameMap, statusNameMap, userMap, false));
        result.put(OUT_HTML_TABLE, buildHtmlTable(list, projectMessageVO, priorityNameMap, statusNameMap, userMap, true));
        return result;
    }

    private String buildHtmlTable(List<IssueDelayCarrierVO> list,
                                  ProjectMessageVO projectMessageVO,
                                  Map<Long, String> priorityNameMap,
                                  Map<Long, String> statusNameMap,
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
                .append("<table style=\"withd:600px\" border=\"1\">")
                .append("<tr><th>问题概要</th><th>编号</th><th>状态</th><th>优先级</th><th>预计开始时间</th>" +
                        "<th>预计结束时间</th><th>经办人</th><th>报告人</th><th>逾期天数</th></tr>");
        Iterator<IssueDelayCarrierVO> iterator = list.iterator();
        while (iterator.hasNext()) {
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
            String priorityName = Optional.ofNullable(priorityNameMap.get(dto.getPriorityId())).orElse("");
            String statusName = Optional.ofNullable(statusNameMap.get(dto.getStatusId())).orElse("");
            String startDate = Optional.ofNullable(dto.getEstimatedStartTime()).map(x -> sdf.format(x)).orElse("");
            String endDate = Optional.ofNullable(dto.getEstimatedEndTime()).map(x -> sdf.format(x)).orElse("");
            String assignee = "";
            if (dto.getAssigneeId() != null) {
                UserDTO user = userMap.get(dto.getAssigneeId());
                if (user != null) {
                    assignee = getNameByLdap(user);
                }
            }
            String reporter = "";
            if (dto.getReporterId() != null) {
                UserDTO user = userMap.get(dto.getReporterId());
                if (user != null) {
                    reporter = getNameByLdap(user);
                }
            }
            builder
                    .append(TR)
                    .append(TD).append("<a href=\"").append(url).append("\" target=\"_blank\">").append(dto.getSummary()).append("</a>").append(BACKSLASH_TD)
                    .append(TD).append(dto.getIssueNum()).append(BACKSLASH_TD)
                    .append(TD).append(statusName).append(BACKSLASH_TD)
                    .append(TD).append(priorityName).append(BACKSLASH_TD)
                    .append(TD).append(startDate).append(BACKSLASH_TD)
                    .append(TD).append(endDate).append(BACKSLASH_TD)
                    .append(TD).append(assignee).append(BACKSLASH_TD)
                    .append(TD).append(reporter).append(BACKSLASH_TD)
                    .append(TD).append(vo.getDelayDay()).append(BACKSLASH_TD)
                    .append(BACKSLASH_TR);
        }
        builder.append("</table>");
        return builder.toString();
    }

    private String getNameByLdap(UserDTO userDTO) {
        if (Boolean.TRUE.equals(userDTO.getLdap())) {
            return userDTO.getRealName() + "（" + userDTO.getLoginName() + "）";
        } else {
            return userDTO.getRealName() + "（" + userDTO.getEmail() + "）";
        }
    }

    private void processProjectOwner(Map<Long, ProjectMessageVO> projectMap, MultiKeyMap multiKeyMap, Map<Long, List<IssueDTO>> issueGroupByProject, Set<Long> userIds, Set<Long> projectIdForProjectOwner, LocalDateTime localDateTime) {
        if (!projectIdForProjectOwner.isEmpty()) {
            List<ProjectWithUserVO> projectWithUserList =
                    baseFeignClient.listProjectOwnerByIds(projectIdForProjectOwner).getBody();
            projectWithUserList.forEach(x -> {
                Long projectId = x.getProjectId();
                userIds.addAll(x.getUserIds());
                x.getUserIds().forEach(y -> {
                    List<IssueDTO> issueList = issueGroupByProject.get(projectId);
                    if (!CollectionUtils.isEmpty(issueList)) {
                        issueList.forEach(z -> {
                            Long organizationId = projectMap.get(projectId).getOrganizationId();
                            LocalDateTime estimatedEndDate = z.getEstimatedEndTime().toInstant().atZone(ZoneId.systemDefault()).toLocalDateTime();

                            long delayDay = estimatedEndDate.isBefore(localDateTime) ?
                                    Duration.between(estimatedEndDate, localDateTime).toDays() + 1 : 0;
                            addValueToMultiKeyMap(
                                    multiKeyMap,
                                    projectId,
                                    y,
                                    new IssueDelayCarrierVO(z, z.getIssueId(), delayDay, organizationId));
                        });
                    }
                });
            });
        }
    }

    private void processIssueToMultiKeyMap(Map<Long, ProjectMessageVO> projectMap,
                                           MultiKeyMap multiKeyMap,
                                           LocalDateTime localDateTime,
                                           Map<Long, List<IssueDTO>> issueGroupByProject,
                                           Set<Long> userIds,
                                           Set<Long> projectIdForProjectOwner,
                                           Map<Long, String> priorityNameMap,
                                           Map<Long, String> statusNameMap) {
        Map<Long, Map<Long, String>> priorityMap = new HashMap<>();
        Map<Long, Map<Long, String>> statusMap = new HashMap<>();
        issueGroupByProject.forEach((k, v) -> {
            Long projectId = k;
            List<IssueDTO> issueList = v;
            ProjectMessageVO projectMessageVO = projectMap.get(projectId);
            Set<String> receiverTypes = projectMessageVO.getReceiverTypes();
            Long organizationId = projectMessageVO.getOrganizationId();
            queryOrganizationPriorityMap(organizationId, priorityMap);
            queryOrganizationStatusMap(organizationId, statusMap);
            issueList.forEach(x -> {
                Date estimatedEndTime = x.getEstimatedEndTime();
                LocalDateTime estimatedEndDate = estimatedEndTime.toInstant().atZone(ZoneId.systemDefault()).toLocalDateTime();
                long delayDay = estimatedEndDate.isBefore(localDateTime) ?
                        Duration.between(estimatedEndDate, localDateTime).toDays() + 1 : 0;
                Map<String, Set<Long>> userTypeMap = new HashMap<>();
                if (receiverTypes != null) {
                    addAssigneeAndReporter(ASSIGNEE, x.getAssigneeId(), userTypeMap, userIds);
                    addAssigneeAndReporter(REPORTER, x.getReporterId(), userTypeMap, userIds);
                    userTypeMap.put(SPECIFIER, projectMessageVO.getUserIds());
                    if (receiverTypes.contains(PROJECT_OWNER)) {
                        projectIdForProjectOwner.add(projectId);
                    }
                    //问题逾期通知增加关注人
                    userTypeMap.put(STAR_USER, new HashSet<>(starBeaconMapper.selectUsersByInstanceId(projectId, x.getIssueId())));
                    addToMultiKeyMap(x, projectMessageVO, delayDay, multiKeyMap, userIds, userTypeMap);
                    //问题逾期通知增加自定义人员字段选项
                    addCustomUserType(organizationId, x, delayDay, receiverTypes, multiKeyMap, userIds);
                }
            });
        });
        priorityMap.forEach((k, v) -> v.forEach(priorityNameMap::put));
        statusMap.forEach((k, v) -> v.forEach(statusNameMap::put));
    }

    private void addCustomUserType(Long organizationId,
                                   IssueDTO issueDTO,
                                   long delayDay,
                                   Set<String> receiverTypes,
                                   MultiKeyMap multiKeyMap,
                                   Set<Long> userIds) {
        List<String> customUserTypes = new ArrayList<>(receiverTypes);
        customUserTypes.removeAll(Arrays.asList(StatusNoticeUserType.BASE_USER_TYPE_LIST));
        if (!CollectionUtils.isEmpty(customUserTypes)) {
            List<Long> customFieldUserIds = fieldValueMapper.selectUserIdByField(issueDTO.getProjectId(), customUserTypes, issueDTO.getIssueId());
            if (!CollectionUtils.isEmpty(customFieldUserIds)) {
                customFieldUserIds.forEach(userId -> {
                    userIds.add(userId);
                    addValueToMultiKeyMap(multiKeyMap,
                            issueDTO.getProjectId(),
                            userId,
                            new IssueDelayCarrierVO(issueDTO, issueDTO.getIssueId(), delayDay, organizationId));
                });
            }
        }
    }

    private void addAssigneeAndReporter(String userType, Long userId, Map<String, Set<Long>> userTypeMap, Set<Long> userIds) {
        if (userId != null && !Objects.equals(0L, userId)) {
            userIds.add(userId);
            userTypeMap.put(userType, Collections.singleton(userId));
        }
    }

    private void addToMultiKeyMap(IssueDTO issueDTO,
                                  ProjectMessageVO projectMessageVO,
                                  long delayDay,
                                  MultiKeyMap multiKeyMap,
                                  Set<Long> userIds,
                                  Map<String,Set<Long>> userTypeMap) {
        Set<String> receiverTypes = projectMessageVO.getReceiverTypes();
        Long organizationId = projectMessageVO.getOrganizationId();

        userTypeMap.forEach((userType, userIdList) -> {
            if (receiverTypes.contains(userType) && !CollectionUtils.isEmpty(userIdList)) {
                userIdList.forEach(userId -> {
                    userIds.add(userId);
                    addValueToMultiKeyMap(multiKeyMap,
                            issueDTO.getProjectId(),
                            userId,
                            new IssueDelayCarrierVO(issueDTO, issueDTO.getIssueId(), delayDay, organizationId));
                });
            }
        });
    }

    private void queryOrganizationStatusMap(Long organizationId,
                                            Map<Long, Map<Long, String>> statusMap) {
        if (ObjectUtils.isEmpty(statusMap.get(organizationId))) {
            Map nameMap = new HashMap();
            statusService.queryAllStatusMap(organizationId).forEach((k, v) -> nameMap.put(k, v.getName()));
            statusMap.put(organizationId, nameMap);
        }
    }

    private void queryOrganizationPriorityMap(Long organizationId,
                                              Map<Long, Map<Long, String>> priorityMap) {
        if (ObjectUtils.isEmpty(priorityMap.get(organizationId))) {
            Map nameMap = new HashMap();
            priorityService.queryByOrganizationId(organizationId).forEach((k, v) -> nameMap.put(k, v.getName()));
            priorityMap.put(organizationId, nameMap);
        }
    }

    private void addValueToMultiKeyMap(MultiKeyMap multiKeyMap,
                                       Long projectId,
                                       Long userId,
                                       IssueDelayCarrierVO issueDelayCarrierVO) {
        Set<IssueDelayCarrierVO> set = (Set<IssueDelayCarrierVO>) multiKeyMap.get(projectId, userId);
        if (set == null) {
            set = new HashSet<>();
            multiKeyMap.put(projectId, userId, set);
        }
        set.add(issueDelayCarrierVO);
    }


}
