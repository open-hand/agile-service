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
import org.slf4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.util.ObjectUtils;

import io.choerodon.agile.api.vo.ProjectMessageVO;
import io.choerodon.agile.api.vo.ProjectWithUserVO;
import io.choerodon.agile.api.vo.SprintDelayCarrierVO;
import io.choerodon.agile.app.service.DelayTaskService;
import io.choerodon.agile.infra.dto.SprintDTO;
import io.choerodon.agile.infra.dto.UserDTO;
import io.choerodon.agile.infra.feign.BaseFeignClient;
import io.choerodon.agile.infra.mapper.SprintMapper;
import io.choerodon.asgard.schedule.annotation.JobTask;
import io.choerodon.asgard.schedule.annotation.TimedTask;
import io.choerodon.asgard.schedule.enums.TriggerTypeEnum;
import io.choerodon.core.enums.MessageAdditionalType;

import org.hzero.boot.message.entity.MessageSender;

/**
 * @author superlee
 * @since 2021-03-02
 */
@Component
public class SprintDelaySendMessageTask {

    private static final Logger LOGGER = getLogger(SprintDelaySendMessageTask.class);

    private static final String SPRINT_DELAY = "SPRINT_DELAY";
    private static final String PROJECT_NAME = "projectName";
    private static final String SPRINT_NAME = "sprintName";
    private static final String DELAY_DAY = "delayDay";
    private static final String START_DATE = "startDate";
    private static final String END_DATE = "endDate";
    private static final String URL = "url";
    private static final String LINK = "link";
    private static final String PROJECT_OWNER = "projectOwner";

    private final SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");

    @Autowired
    private SprintMapper sprintMapper;
    @Autowired
    private BaseFeignClient baseFeignClient;
    @Autowired
    private DelayTaskService delayTaskService;

    @Value("${services.domain.url}")
    private String domainUrl;

    @JobTask(
            maxRetryCount = 3,
            code = "sprintDelaySendMessage",
            description = "冲刺延期，发送消息给指定人"
    )
    @TimedTask(
            name = "sprintDelaySendMessage",
            description = "冲刺延期，发送消息给指定人",
            oneExecution = false,
            params = {},
            triggerType = TriggerTypeEnum.CRON_TRIGGER,
            cronExpression = "0 30 3 * * ? "
    )
    public void run(Map<String, Object> map) {
        LOGGER.info("===> 开始执行冲刺延期发送消息定时任务");
        Map<Long, ProjectMessageVO> projectMap = delayTaskService.listEnabledMsgProjects(SPRINT_DELAY);
        if (ObjectUtils.isEmpty(projectMap)) {
            LOGGER.info("===> 没有配置发送消息的项目，冲刺延期发送消息定时任务完成");
            return;
        }
        Set<Long> projectIds = projectMap.keySet();
        List<SprintDelayCarrierVO> sprintDelayCarrierList = new ArrayList<>();
        if (!ObjectUtils.isEmpty(projectIds)) {
            List<SprintDTO> sprints = sprintMapper.selectActiveSprintsByProjectIds(projectIds);
            for (SprintDTO x : sprints) {
                Date endDate = x.getEndDate();
                if (!ObjectUtils.isEmpty(endDate)) {
                    LocalDateTime now = LocalDateTime.now();
                    LocalDateTime end = endDate.toInstant().atZone(ZoneId.systemDefault()).toLocalDateTime();
                    if (now.isAfter(end)) {
                        long delayDay = end.isBefore(now) ?
                                Duration.between(end, now).toDays() + 1 : 0;
                        Long projectId = x.getProjectId();
                        ProjectMessageVO projectMessageVO = projectMap.get(projectId);
                        String projectName = null;
                        Long organizationId = null;
                        if (projectMessageVO != null) {
                            projectName = projectMessageVO.getName();
                            organizationId = projectMessageVO.getOrganizationId();
                        }
                        SprintDelayCarrierVO sprintDelayCarrierVO = new SprintDelayCarrierVO(projectId, x, delayDay, projectName, organizationId);
                        sprintDelayCarrierList.add(sprintDelayCarrierVO);
                    }
                }
            }
        }
        Map<Long, UserDTO> userMap = new HashMap<>();
        Map<Long, Set<Long>> projectOwnerMap = new HashMap<>();
        processUsers(userMap, sprintDelayCarrierList, projectMap, projectOwnerMap);
        List<MessageSender> messageSenders = buildMessageSender(sprintDelayCarrierList, userMap, projectOwnerMap);
        // 批量发送通知
        delayTaskService.batchSendMessage(messageSenders, 500);
        LOGGER.info("===> 冲刺延期发送消息定时任务完成");
    }

    private void processUsers(Map<Long, UserDTO> userMap,
                              List<SprintDelayCarrierVO> sprintDelayCarrierList,
                              Map<Long, ProjectMessageVO> projectMap,
                              Map<Long, Set<Long>> projectOwnerMap) {
        Set<Long> userIds = new HashSet<>();
        Set<Long> projectIdForProjectOwner = new HashSet<>();
        for (SprintDelayCarrierVO sprintDelayCarrierVO : sprintDelayCarrierList) {
            Long projectId = sprintDelayCarrierVO.getProjectId();
            ProjectMessageVO projectMessageVO = projectMap.get(projectId);
            Set<Long> userIdSet = new HashSet<>();
            sprintDelayCarrierVO.setUserIds(userIdSet);
            if (!ObjectUtils.isEmpty(projectMessageVO.getUserIds())) {
                userIdSet.addAll(projectMessageVO.getUserIds());
                userIds.addAll(projectMessageVO.getUserIds());
            }
            Set<String> receiverTypes = projectMessageVO.getReceiverTypes();
            if (receiverTypes != null && receiverTypes.contains(PROJECT_OWNER)) {
                projectIdForProjectOwner.add(projectId);
            }
        }
        if (CollectionUtils.isNotEmpty(projectIdForProjectOwner)) {
            List<ProjectWithUserVO> projectWithUserList =
                    baseFeignClient.listProjectOwnerByIds(projectIdForProjectOwner).getBody();
            if(CollectionUtils.isNotEmpty(projectWithUserList)) {
                for (ProjectWithUserVO x : projectWithUserList) {
                    projectOwnerMap.computeIfAbsent(x.getProjectId(), y -> x.getUserIds());
                    userIds.addAll(x.getUserIds());
                }
            }
        }
        if (!userIds.isEmpty()) {
            userMap.putAll(
                    Objects.requireNonNull(baseFeignClient.listUsersByIds(userIds.toArray(new Long[0]), true)
                                    .getBody())
                            .stream()
                            .collect(Collectors.toMap(UserDTO::getId, Function.identity()))
            );
        }
    }

    private List<MessageSender> buildMessageSender(List<SprintDelayCarrierVO> sprintDelayCarrierList,
                                                   Map<Long, UserDTO> userMap,
                                                   Map<Long, Set<Long>> projectOwnerMap) {
        List<MessageSender> messageSenders = new ArrayList<>();
        for (SprintDelayCarrierVO x : sprintDelayCarrierList) {
            Long projectId = x.getProjectId();
            SprintDTO sprint = x.getSprintDTO();
            Long delayDay = x.getDelayDay();
            if (ObjectUtils.isEmpty(sprint)
                    || ObjectUtils.isEmpty(sprint.getStartDate())
                    || ObjectUtils.isEmpty(sprint.getEndDate())) {
                continue;
            }
            String url = buildSprintDelayUrl(projectId, x.getProjectName(), x.getOrganizationId());
            Map<String, String> paramMap = new HashMap<>(7);
            paramMap.put(PROJECT_NAME, x.getProjectName());
            paramMap.put(SPRINT_NAME, sprint.getSprintName());
            paramMap.put(DELAY_DAY, String.valueOf(delayDay));
            paramMap.put(START_DATE, sdf.format(sprint.getStartDate()));
            paramMap.put(END_DATE, sdf.format(sprint.getEndDate()));
            paramMap.put(URL, url);
            paramMap.put(LINK, domainUrl + url);
            List<UserDTO> users = generateUser(x.getUserIds(), userMap, projectOwnerMap.get(projectId));
            MessageSender messageSender = delayTaskService.buildSender(0L, SPRINT_DELAY, paramMap, users);
            Map<String, Object> additionalInformationMap = new HashMap<>(1);
            additionalInformationMap.put(MessageAdditionalType.PARAM_PROJECT_ID.getTypeName(), projectId);
            messageSender.setAdditionalInformation(additionalInformationMap);
            messageSenders.add(messageSender);

        }
        return messageSenders;
    }

    private String buildSprintDelayUrl(Long projectId,
                                       String projectName,
                                       Long organizationId) {
        return "/#/agile/scrumboard?type=project&id=" +
                projectId +
                "&name=" +
                projectName +
                "&organizationId=" +
                organizationId;
    }

    private List<UserDTO> generateUser(Set<Long> userIds,
                                       Map<Long, UserDTO> userMap,
                                       Set<Long> projectOwnerIds) {
        List<UserDTO> users = new ArrayList<>();
        processUserIdsToDtoList(userIds, userMap, users);
        processUserIdsToDtoList(projectOwnerIds, userMap, users);
        return users;
    }

    private void processUserIdsToDtoList(Set<Long> userIds,
                                         Map<Long, UserDTO> userMap,
                                         List<UserDTO> users) {
        if (!ObjectUtils.isEmpty(userIds)) {
            for (Long x : userIds) {
                UserDTO dto = userMap.get(x);
                if (dto != null) {
                    users.add(dto);
                }
            }
        }
    }
}
