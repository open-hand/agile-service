package io.choerodon.agile.infra.utils;

import io.choerodon.agile.api.vo.IssueCommentVO;
import io.choerodon.agile.api.vo.ProjectVO;
import io.choerodon.agile.app.service.UserService;
import io.choerodon.agile.infra.dto.ProjectReportReceiverDTO;
import io.choerodon.agile.infra.dto.UserDTO;
import io.choerodon.agile.infra.dto.UserMessageDTO;
import io.choerodon.agile.infra.feign.BaseFeignClient;
import io.choerodon.core.enums.MessageAdditionalType;
import org.apache.commons.collections4.CollectionUtils;
import org.hzero.boot.message.MessageClient;
import org.hzero.boot.message.entity.MessageSender;
import org.hzero.boot.message.entity.Receiver;
import org.hzero.core.base.BaseConstants;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Component;
import org.springframework.util.Assert;
import org.springframework.util.ObjectUtils;

import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * Created by HuangFuqiang@choerodon.io on 2018/10/8.
 * Email: fuqianghuang01@gmail.com
 */
@Component
public class SiteMsgUtil {

    private static final String ASSIGNEENAME = "assigneeName";
    private static final String OPERATOR_NAME = "operatorName";
    private static final String SUMMARY = "summary";
    private static final String URL = "url";
    private static final String PROJECT_NAME = "projectName";
    private static final String USER_NAME = "userName";
    private static final String ISSUE_SUMMARY = "issueSummary";
    private static final String LINK = "link";
    private static final String COMMENT = "comment";
    private static final String ACTION = "action";
    private static final String COMMENT_USER = "commentUser";
    private static final String COMMENT_TYPE = "commentType";
    private static final String ISSUE_TYPE = "issueType";
    private static final String LOGIN_NAME = "loginName";

    @Autowired
    private BaseFeignClient baseFeignClient;
    @Autowired
    private UserService userService;
    @Autowired
    private MessageClient messageClient;
    @Value("${services.domain.url}")
    private String domainUrl;

    public void issueCreate(List<Long> userIds,
                            String userName,
                            String summary,
                            String url,
                            Long operatorId,
                            Long projectId,
                            boolean customFieldUsers) {
        ProjectVO projectVO = baseFeignClient.queryProject(projectId).getBody();
        Map<String,String> map = new HashMap<>();
        map.put(ASSIGNEENAME, userName);
        map.put(SUMMARY, summary);
        map.put(URL, url);
        map.put(PROJECT_NAME, projectVO.getName());
        setLoginNameAndRealName(operatorId, map);
        // 设置额外参数
        Map<String,Object> objectMap=new HashMap<>();
        objectMap.put(MessageAdditionalType.PARAM_PROJECT_ID.getTypeName(),projectId);
        //发送站内信
        MessageSender messageSender = handlerMessageSender(0L,"ISSUECREATE",userIds,map);
        messageSender.setAdditionalInformation(objectMap);
        //问题创建通知自定义字段人员时仅发送邮件和站内信，避免重复发送webhook
        if (customFieldUsers) {
            messageClient.async().sendWebMessage("ISSUE_CREATE.WEB", messageSender.getReceiverAddressList(), messageSender.getArgs());
            messageClient.async().sendEmail("CHOERODON-EMAIL", "ISSUE_CREATE.EMAIL", messageSender.getReceiverAddressList(), messageSender.getArgs());
        } else {
            messageClient.async().sendMessage(messageSender);
        }
    }

    private void setLoginNameAndRealName(Long operatorId, Map<String, String> map) {
        UserDTO operator = queryUserById(operatorId);
        if (operator != null) {
            map.put(LOGIN_NAME, operator.getLoginName());
            map.put(USER_NAME, operator.getRealName());
        }
    }

    private UserDTO queryUserById(Long operatorId) {
        List<UserDTO> users =
                baseFeignClient.listUsersByIds(Arrays.asList(operatorId).toArray(new Long[1]), true).getBody();
        if (ObjectUtils.isEmpty(users)) {
            return null;
        }
        return users.get(0);
    }

    private MessageSender handlerMessageSender(Long tenantId,String messageCode,List<Long> userIds,Map<String,String> map){
        MessageSender messageSender = new MessageSender();
        messageSender.setTenantId(tenantId);
        messageSender.setMessageCode(messageCode);
        List<Receiver> receivers = new ArrayList<>();
        handleReceiver(receivers,userIds);
        // 设置参数
        messageSender.setArgs(map);
        // 设置接收者
        messageSender.setReceiverAddressList(receivers);
        return messageSender;
    }

    public Map<Long, UserDTO> handleReceiver(List<Receiver> receivers,Collection<Long> userIds){
        if (CollectionUtils.isEmpty(userIds)){
            return new HashMap<>();
        }
        List<UserDTO> users = baseFeignClient.listUsersByIds(userIds.toArray(new Long[]{}), true).getBody();
        if (CollectionUtils.isEmpty(users)){
            return new HashMap<>();
        }
        Map<Long, UserDTO> userDTOMap = users.stream().collect(Collectors.toMap(UserDTO::getId, Function.identity()));
        // 未启用用户则不进行发消息
        for (Map.Entry<Long, UserDTO> entry : userDTOMap.entrySet()) {
            Receiver receiver = new Receiver();
            UserDTO userDTO = entry.getValue();
            receiver.setUserId(userDTO.getId());
            receiver.setEmail(userDTO.getEmail());
            receiver.setPhone(userDTO.getPhone());
            receiver.setTargetUserTenantId(userDTO.getOrganizationId());
            receivers.add(receiver);
        }
        return userDTOMap;
    }

    public void issueAssignee(List<Long> userIds,
                              String assigneeName,
                              String summary,
                              String url,
                              Long projectId,
                              String operatorName,
                              Long operatorId) {
        // 设置模板参数
        ProjectVO projectVO = baseFeignClient.queryProject(projectId).getBody();
        Map<String,String> map = new HashMap<>();
        map.put(ASSIGNEENAME, assigneeName);
        map.put(SUMMARY, summary);
        map.put(URL, url);
        map.put(PROJECT_NAME, projectVO.getName());
        map.put(OPERATOR_NAME, operatorName);
        setLoginNameAndRealName(operatorId, map);
        // 额外参数
        Map<String,Object> objectMap=new HashMap<>();
        objectMap.put(MessageAdditionalType.PARAM_PROJECT_ID.getTypeName(),projectId);
        //发送站内信
        MessageSender messageSender = handlerMessageSender(0L,"ISSUEASSIGNEE",userIds,map);
        messageSender.setAdditionalInformation(objectMap);
        messageClient.async().sendMessage(messageSender);
    }

    public void issueSolve(List<Long> userIds,
                           String assigneeName,
                           String summary,
                           String url,
                           Long projectId,
                           String operatorName,
                           Long operatorId) {
        ProjectVO projectVO = baseFeignClient.queryProject(projectId).getBody();
        Map<String,String> map = new HashMap<>();
        map.put(ASSIGNEENAME, assigneeName);
        map.put(OPERATOR_NAME, operatorName);
        map.put(SUMMARY, summary);
        map.put(URL, url);
        map.put(PROJECT_NAME, projectVO.getName());
        setLoginNameAndRealName(operatorId, map);
        // 额外参数
        Map<String,Object> objectMap=new HashMap<>();
        objectMap.put(MessageAdditionalType.PARAM_PROJECT_ID.getTypeName(),projectId);
        //发送站内信
        MessageSender messageSender = handlerMessageSender(0L,"ISSUESOLVE",userIds,map);
        messageSender.setAdditionalInformation(objectMap);
        messageClient.async().sendMessage(messageSender);
    }

    public void sendChangeIssueStatus(Long projectId,
                                      Set<Long> userSet,
                                      List<String> noticeTypeList,
                                      Map<String, String> templateArgsMap,
                                      Long operatorId,
                                      boolean onlyWebHook){
        MessageSender messageSender = new MessageSender();
        messageSender.setTenantId(BaseConstants.DEFAULT_TENANT_ID);
        messageSender.setMessageCode("ISSUECHANGESTATUS");
        List<Receiver> receiverList = new ArrayList<>();
        handleReceiver(receiverList, userSet);
        setLoginNameAndRealName(operatorId, templateArgsMap);
        // 设置模板参数
        messageSender.setArgs(templateArgsMap);
        // 设置额外参数
        Map<String,Object> objectMap=new HashMap<>();
        objectMap.put(MessageAdditionalType.PARAM_PROJECT_ID.getTypeName(),projectId);
        messageSender.setAdditionalInformation(objectMap);
        //如果noticeTypeList仅有WebHook不需要receiver
        if (!onlyWebHook && CollectionUtils.isEmpty(receiverList)) {
            return;
        }
        messageSender.setTypeCodeList(noticeTypeList);
        messageSender.setReceiverAddressList(receiverList);
        messageClient.async().sendMessage(messageSender);
    }

    @Async
    public void sendProjectReport(Long projectId, List<ProjectReportReceiverDTO> receiverList, String html) {
        // 获取接收人, 抄送人
        Map<String, List<ProjectReportReceiverDTO>> group =
                receiverList.stream().collect(Collectors.groupingBy(ProjectReportReceiverDTO::getType));
        List<Long> toList = group.get(ProjectReportReceiverDTO.TYPE_RECEIVER).stream()
                .map(ProjectReportReceiverDTO::getReceiverId).collect(Collectors.toList());
        Assert.notNull(toList, BaseConstants.ErrorCode.DATA_NOT_EXISTS);
        List<Long> ccList = group.getOrDefault(ProjectReportReceiverDTO.TYPE_CC, Collections.emptyList()).stream()
                .map(ProjectReportReceiverDTO::getReceiverId).collect(Collectors.toList());
        List<Receiver> toReceiver = new ArrayList<>();
        List<Receiver> ccReceiver = new ArrayList<>();
        handleReceiver(toReceiver, toList);
        Assert.isTrue(CollectionUtils.isNotEmpty(toList), BaseConstants.ErrorCode.DATA_NOT_EXISTS);
        handleReceiver(ccReceiver, ccList);
        // 设置参数
        Map<String, String> argsMap = new HashMap<>();
        argsMap.put("data", html);
        // 设置sender
        MessageSender sender = new MessageSender();
        sender.setMessageCode("PROJECT_REPORT");
        sender.setTenantId(ConvertUtil.getOrganizationId(projectId));
        sender.setReceiverAddressList(toReceiver);
        sender.setCcList(ccReceiver.stream().map(Receiver::getEmail).collect(Collectors.toList()));
        sender.setArgs(argsMap);
        messageClient.async().sendMessage(sender);
    }

    public MessageSender issueCreateSender(List<Long> userIds,
                                           String userName,
                                           String summary,
                                           String url,
                                           Long projectId,
                                           Long operatorId) {
        ProjectVO projectVO = baseFeignClient.queryProject(projectId).getBody();
        Map<String,String> map = new HashMap<>();
        map.put(ASSIGNEENAME, userName);
        map.put(SUMMARY, summary);
        map.put(URL, url);
        map.put(PROJECT_NAME, projectVO.getName());
        setLoginNameAndRealName(operatorId, map);
        // 设置额外参数
        Map<String,Object> objectMap=new HashMap<>();
        objectMap.put(MessageAdditionalType.PARAM_PROJECT_ID.getTypeName(),projectId);
        //发送站内信
        MessageSender messageSender = handlerMessageSender(0L,"ISSUECREATE",userIds,map);
        messageSender.setAdditionalInformation(objectMap);
        return messageSender;
    }

    public MessageSender issueAssigneeSender(List<Long> userIds,
                                             String assigneeName,
                                             String summary,
                                             String url,
                                             Long projectId,
                                             String operatorName,
                                             Long operatorId) {
        // 设置模板参数
        ProjectVO projectVO = baseFeignClient.queryProject(projectId).getBody();
        Map<String,String> map = new HashMap<>();
        map.put(ASSIGNEENAME, assigneeName);
        map.put(SUMMARY, summary);
        map.put(URL, url);
        map.put(PROJECT_NAME, projectVO.getName());
        map.put(OPERATOR_NAME, operatorName);
        setLoginNameAndRealName(operatorId, map);
        // 额外参数
        Map<String,Object> objectMap=new HashMap<>();
        objectMap.put(MessageAdditionalType.PARAM_PROJECT_ID.getTypeName(),projectId);
        //发送站内信
        MessageSender messageSender = handlerMessageSender(0L,"ISSUEASSIGNEE",userIds,map);
        messageSender.setAdditionalInformation(objectMap);
        return messageSender;
    }

    public MessageSender issueSolveSender(List<Long> userIds,
                                          String assigneeName,
                                          String summary,
                                          String url,
                                          Long projectId,
                                          String operatorName,
                                          Long operatorId) {
        ProjectVO projectVO = baseFeignClient.queryProject(projectId).getBody();
        Map<String,String> map = new HashMap<>();
        map.put(ASSIGNEENAME, assigneeName);
        map.put(OPERATOR_NAME, operatorName);
        map.put(SUMMARY, summary);
        map.put(URL, url);
        map.put(PROJECT_NAME, projectVO.getName());
        setLoginNameAndRealName(operatorId, map);
        // 额外参数
        Map<String,Object> objectMap=new HashMap<>();
        objectMap.put(MessageAdditionalType.PARAM_PROJECT_ID.getTypeName(),projectId);
        //发送站内信
        MessageSender messageSender = handlerMessageSender(0L,"ISSUESOLVE",userIds,map);
        messageSender.setAdditionalInformation(objectMap);
        return messageSender;
    }

    public MessageSender sendChangeIssueStatusSender(Long projectId,
                                                     Set<Long> userSet,
                                                     List<String> noticeTypeList,
                                                     Map<String, String> templateArgsMap,
                                                     Long operatorId){
        MessageSender messageSender = new MessageSender();
        messageSender.setTenantId(BaseConstants.DEFAULT_TENANT_ID);
        messageSender.setMessageCode("ISSUECHANGESTATUS");
        List<Receiver> receiverList = new ArrayList<>();
        setLoginNameAndRealName(operatorId, templateArgsMap);
        // 设置模板参数
        messageSender.setArgs(templateArgsMap);
        // 设置额外参数
        Map<String,Object> objectMap=new HashMap<>();
        objectMap.put(MessageAdditionalType.PARAM_PROJECT_ID.getTypeName(),projectId);
        messageSender.setAdditionalInformation(objectMap);
        if (CollectionUtils.isEmpty(receiverList)) {
            return null;
        }
        messageSender.setTypeCodeList(noticeTypeList);
        messageSender.setReceiverAddressList(receiverList);
        return messageSender;
    }

    //发送问题评论消息
    public void sendIssueComment(Map<Long, String> actionMap,
                                 ProjectVO projectVO,
                                 String summary,
                                 String url,
                                 String comment,
                                 IssueCommentVO issueCommentVO,
                                 String issueType,
                                 Long operatorId) {
        List<MessageSender> senderList = new ArrayList<>();
        List<Receiver> receiverList = new ArrayList<>();
        handleReceiver(receiverList, actionMap.keySet());
        Map<Long, Receiver> usersMap = receiverList.stream().collect(Collectors.toMap(Receiver::getUserId, Function.identity()));
        boolean replyAble = issueCommentVO.getParentId() != null && issueCommentVO.getParentId() != 0L;
        UserDTO operator = queryUserById(operatorId);
        actionMap.forEach((userId, action) -> {
            Map<String, String> argsMap = new HashMap<>(9);
            argsMap.put(PROJECT_NAME, projectVO.getName());
            argsMap.put(ISSUE_SUMMARY, summary);
            argsMap.put(LINK, domainUrl + "/" + url);
            argsMap.put(URL, url);
            argsMap.put(COMMENT, comment);
            argsMap.put(ACTION, action);
            argsMap.put(COMMENT_USER, issueCommentVO.getUserName());
            argsMap.put(COMMENT_TYPE, replyAble ? "回复" : "评论");
            argsMap.put(ISSUE_TYPE, issueType);
            if (operator != null) {
                argsMap.put(LOGIN_NAME, operator.getLoginName());
                argsMap.put(USER_NAME, operator.getRealName());
            }

            MessageSender messageSender = new MessageSender();
            messageSender.setTenantId(ConvertUtil.getOrganizationId(projectVO.getId()));
            messageSender.setMessageCode("ISSUE_COMMENT");
            messageSender.setReceiverAddressList(Objects.isNull(userId) ? null : Collections.singletonList(usersMap.get(userId)));
            messageSender.setArgs(argsMap);
            // 设置额外参数
            Map<String, Object> objectMap = new HashMap<>(1);
            objectMap.put(MessageAdditionalType.PARAM_PROJECT_ID.getTypeName(), projectVO.getId());
            messageSender.setAdditionalInformation(objectMap);
            senderList.add(messageSender);
        });
        senderList.forEach(sender -> {
            if (!Objects.isNull(sender.getReceiverAddressList())) {
                //向接收人发送邮件和站内信消息
                messageClient.async().sendWebMessage("ISSUE_COMMENT_NOTICE.WEB", sender.getReceiverAddressList(), sender.getArgs());
                messageClient.async().sendEmail("CHOERODON-EMAIL", "ISSUE_COMMENT_NOTICE.EMAIL", sender.getReceiverAddressList(), sender.getArgs());
            } else {
                //发送无接收人的消息(webhook等)
                messageClient.async().sendMessage(sender);
            }
        });
    }

    public void issueParticipant(String summary, String url, Long projectId, Long operatorId, List<Long> sendUserIds, String operatorName, List<Long> participantIds) {
        Map<String,String> map = new HashMap<>();
        map.put(SUMMARY, summary);
        map.put(OPERATOR_NAME, operatorName);
        map.put(URL, url);
        map.put(LINK, domainUrl + "/" + url);
        setLoginNameAndRealName(operatorId, map);
        setParticipantName(map, participantIds);
        // 额外参数
        Map<String,Object> objectMap=new HashMap<>();
        objectMap.put(MessageAdditionalType.PARAM_PROJECT_ID.getTypeName(),projectId);
        //发送站内信
        MessageSender messageSender = handlerMessageSender(0L,"ISSUE_SET_PARTICIPANT",sendUserIds, map);
        messageSender.setAdditionalInformation(objectMap);
        messageClient.async().sendMessage(messageSender);
    }

    private void setParticipantName(Map<String,String> map, List<Long> participantIds) {
        Map<Long, UserMessageDTO> userMessageDTOMap = userService.queryUsersMap(participantIds, true);
        if (CollectionUtils.isNotEmpty(participantIds)) {
            String participantName = participantIds.stream().map(v -> userMessageDTOMap.get(v))
                                        .filter(Objects::nonNull)
                                        .map(UserMessageDTO::getName)
                                        .collect(Collectors.joining(","));
            map.put("participantName", participantName);
        }
    }
}
