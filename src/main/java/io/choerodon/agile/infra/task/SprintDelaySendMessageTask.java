package io.choerodon.agile.infra.task;

import io.choerodon.agile.api.vo.ProjectMessageVO;
import io.choerodon.agile.api.vo.ProjectWithUserVO;
import io.choerodon.agile.api.vo.SprintDelayCarrierVO;
import io.choerodon.agile.infra.dto.SprintDTO;
import io.choerodon.agile.infra.dto.UserDTO;
import io.choerodon.agile.infra.feign.BaseFeignClient;
import io.choerodon.agile.infra.feign.NotifyFeignClient;
import io.choerodon.agile.infra.mapper.SprintMapper;
import io.choerodon.asgard.schedule.annotation.JobTask;
import io.choerodon.asgard.schedule.annotation.TimedTask;
import io.choerodon.asgard.schedule.enums.TriggerTypeEnum;
import io.choerodon.core.enums.MessageAdditionalType;
import org.hzero.boot.message.MessageClient;
import org.hzero.boot.message.entity.MessageSender;
import org.hzero.boot.message.entity.Receiver;
import org.slf4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.ObjectUtils;

import java.text.SimpleDateFormat;
import java.time.LocalDate;
import java.time.Period;
import java.time.ZoneId;
import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

import static org.slf4j.LoggerFactory.getLogger;

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
    private static final String PROJECT_OWNER = "projectOwner";

    private static final SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");

    @Autowired
    private NotifyFeignClient notifyFeignClient;
    @Autowired
    private SprintMapper sprintMapper;
    @Autowired
    private MessageClient messageClient;
    @Autowired
    private BaseFeignClient baseFeignClient;

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
            cronExpression = "0 0 3 * * ? "
    )
    public void run(Map<String, Object> map) {
        LOGGER.info("===> 开始执行冲刺延期发送消息定时任务");
        //获取所有配置过冲刺延期发送消息的项目
        List<ProjectMessageVO> projectMessageList =
                notifyFeignClient.listEnabledSettingByCode(SPRINT_DELAY, "agile").getBody();
        if (ObjectUtils.isEmpty(projectMessageList)) {
            LOGGER.info("===> 没有配置发送消息的项目，冲刺延期发送消息定时任务完成");
            return;
        }
        Map<Long, ProjectMessageVO> projectMap =
                projectMessageList
                        .stream()
                        .collect(Collectors.toMap(ProjectMessageVO::getId, Function.identity()));
        Set<Long> projectIds = projectMap.keySet();
        List<SprintDelayCarrierVO> sprintDelayCarrierList = new ArrayList<>();
        if (!ObjectUtils.isEmpty(projectIds)) {
            List<SprintDTO> sprints = sprintMapper.selectActiveSprintsByProjectIds(projectIds);
            sprints.forEach(x -> {
                Date endDate = x.getEndDate();
                if (!ObjectUtils.isEmpty(endDate)) {
                    LocalDate now = LocalDate.now();
                    LocalDate end = endDate.toInstant().atZone(ZoneId.systemDefault()).toLocalDate();
                    if (now.isAfter(end)) {
                        int delayDas = Period.between(end, now).getDays();
                        Long projectId = x.getProjectId();
                        ProjectMessageVO projectMessageVO = projectMap.get(projectId);
                        String projectName = null;
                        Long organizationId = null;
                        if (projectMessageVO != null) {
                            projectName = projectMessageVO.getName();
                            organizationId = projectMessageVO.getOrganizationId();
                        }
                        SprintDelayCarrierVO sprintDelayCarrierVO = new SprintDelayCarrierVO(projectId, x, delayDas, projectName, organizationId);
                        sprintDelayCarrierList.add(sprintDelayCarrierVO);
                    }
                }
            });
        }
        Map<Long, UserDTO> userMap = new HashMap<>();
        Map<Long, Set<Long>> projectOwnerMap = new HashMap<>();
        processUsers(userMap, sprintDelayCarrierList, projectMap, projectOwnerMap);
        List<MessageSender> messageSenders = buildMessageSender(sprintDelayCarrierList, userMap, projectOwnerMap);
        if(!messageSenders.isEmpty()) {
            messageSenders
                    .stream()
                    .parallel()
                    .forEach(x -> messageClient.async().sendMessage(x));
        }
        LOGGER.info("===> 冲刺延期发送消息定时任务完成");
    }

    private void processUsers(Map<Long, UserDTO> userMap,
                              List<SprintDelayCarrierVO> sprintDelayCarrierList,
                              Map<Long, ProjectMessageVO> projectMap,
                              Map<Long, Set<Long>> projectOwnerMap) {
        Set<Long> userIds = new HashSet<>();
        Set<Long> projectIdForProjectOwner = new HashSet<>();
        sprintDelayCarrierList.forEach(x -> {
            Long projectId = x.getProjectId();
            ProjectMessageVO projectMessageVO = projectMap.get(projectId);
            Set<Long> userIdSet = new HashSet<>();
            x.setUserIds(userIdSet);
            if (!ObjectUtils.isEmpty(projectMessageVO.getUserIds())) {
                userIdSet.addAll(projectMessageVO.getUserIds());
                userIds.addAll(projectMessageVO.getUserIds());
            }
            Set<String> receiverTypes = projectMessageVO.getReceiverTypes();
            if (receiverTypes.contains(PROJECT_OWNER)) {
                projectIdForProjectOwner.add(projectId);
            }
        });
        if (!projectIdForProjectOwner.isEmpty()) {
            List<ProjectWithUserVO> projectWithUserList =
                    baseFeignClient.listProjectOwnerByIds(projectIdForProjectOwner).getBody();
            projectWithUserList.forEach(x -> {
                projectOwnerMap.computeIfAbsent(x.getProjectId(), y -> x.getUserIds());
                userIds.addAll(x.getUserIds());
            });
        }
        if (!userIds.isEmpty()) {
            userMap.putAll(
                    baseFeignClient.listUsersByIds(userIds.toArray(new Long[userIds.size()]), true)
                            .getBody()
                            .stream()
                            .collect(Collectors.toMap(UserDTO::getId, Function.identity()))
            );
        }
    }

    private List<MessageSender> buildMessageSender(List<SprintDelayCarrierVO> sprintDelayCarrierList,
                                                   Map<Long, UserDTO> userMap,
                                                   Map<Long, Set<Long>> projectOwnerMap) {
        List<MessageSender> messageSenders = new ArrayList<>();
        sprintDelayCarrierList.forEach(x -> {
            Long projectId = x.getProjectId();
            SprintDTO sprint = x.getSprintDTO();
            Integer delayDay = x.getDelayDay();
            if (ObjectUtils.isEmpty(sprint)
                    || ObjectUtils.isEmpty(sprint.getStartDate())
                    || ObjectUtils.isEmpty(sprint.getEndDate())) {
                return;
            }
            Map<String, String> paramMap = new HashMap<>();
            paramMap.put(PROJECT_NAME, x.getProjectName());
            paramMap.put(SPRINT_NAME, sprint.getSprintName());
            paramMap.put(DELAY_DAY, String.valueOf(delayDay));
            paramMap.put(START_DATE, sdf.format(sprint.getStartDate()));
            paramMap.put(END_DATE, sdf.format(sprint.getEndDate()));
            paramMap.put(URL, buildSprintDelayUrl(projectId, x.getProjectName(), x.getOrganizationId()));
            List<UserDTO> users = generateUser(x.getUserIds(), userMap, projectOwnerMap.get(projectId));
            MessageSender messageSender = buildSender(0L, SPRINT_DELAY, paramMap, users);
            Map<String, Object> additionalInformationMap = new HashMap<>();
            additionalInformationMap.put(MessageAdditionalType.PARAM_PROJECT_ID.getTypeName(), projectId);
            messageSender.setAdditionalInformation(additionalInformationMap);
            messageSenders.add(messageSender);

        });
        return messageSenders;
    }

    private String buildSprintDelayUrl(Long projectId,
                                       String projectName,
                                       Long organizationId) {
        StringBuilder builder = new StringBuilder();
        builder
                .append("/#/agile/scrumboard?type=project&id=")
                .append(projectId)
                .append("&name=")
                .append(projectName)
                .append("&category=AGILE&organizationId=")
                .append(organizationId);
        return builder.toString();
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
            userIds.forEach(x -> {
                UserDTO dto = userMap.get(x);
                if (dto != null) {
                    users.add(dto);
                }
            });
        }
    }

    public MessageSender buildSender(Long tenantId,
                                     String messageCode,
                                     Map<String, String> paramMap,
                                     List<UserDTO> users) {
        MessageSender messageSender = new MessageSender();
        messageSender.setTenantId(tenantId);
        messageSender.setMessageCode(messageCode);
        List<Receiver> receivers = buildReceivers(users);
        // 设置参数
        messageSender.setArgs(paramMap);
        // 设置接收者
        messageSender.setReceiverAddressList(receivers);
        return messageSender;
    }

    private List<Receiver> buildReceivers(List<UserDTO> users) {
        List<Receiver> receivers = new ArrayList<>();
        if (ObjectUtils.isEmpty(users)) {
            return receivers;
        }
        users.forEach(x -> {
            Receiver receiver = new Receiver();
            receivers.add(receiver);
            receiver.setUserId(x.getId());
            receiver.setEmail(x.getEmail());
            receiver.setPhone(x.getPhone());
            receiver.setTargetUserTenantId(x.getOrganizationId());
        });
        return receivers;
    }
}
