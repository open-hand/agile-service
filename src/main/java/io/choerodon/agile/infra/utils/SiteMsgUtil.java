package io.choerodon.agile.infra.utils;

import io.choerodon.agile.api.vo.ProjectVO;
import io.choerodon.agile.app.service.UserService;
import io.choerodon.agile.infra.dto.UserDTO;
import io.choerodon.agile.infra.feign.BaseFeignClient;
//import io.choerodon.agile.infra.feign.NotifyFeignClient;
//import io.choerodon.core.notify.NoticeSendDTO;
import org.hzero.boot.message.MessageClient;
import org.hzero.boot.message.entity.MessageSender;
import org.hzero.boot.message.entity.Receiver;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * Created by HuangFuqiang@choerodon.io on 2018/10/8.
 * Email: fuqianghuang01@gmail.com
 */
@Component
public class SiteMsgUtil {

    private static Logger LOGGER = LoggerFactory.getLogger(SiteMsgUtil.class);

    private static final String ASSIGNEENAME = "assigneeName";
    private static final String SUMMARY = "summary";
    private static final String URL = "url";
    private static final String NOTIFY_TYPE = "agile";
    private static final String PROJECT_NAME = "projectName";

//    @Autowired
//    private NotifyFeignClient notifyFeignClient;
    @Autowired
    private BaseFeignClient baseFeignClient;
    @Autowired
    private UserService userService;
    @Autowired
    private MessageClient messageClient;

    public void issueCreate(List<Long> userIds,String userName, String summary, String url, Long reporterId, Long projectId) {
//        NoticeSendDTO noticeSendDTO = new NoticeSendDTO();
//        // 添加普通消息
//        setCommonMsg(noticeSendDTO, projectId, "issueCreate", userName, summary, url, userIds, reporterId);
//        // 添加webhook
//        setWebHookJson(noticeSendDTO, noticeSendDTO.getParams(), reporterId, "issueCreate", "问题创建");
//        try {
//            notifyFeignClient.postNotice(noticeSendDTO);
//        } catch (Exception e) {
//            LOGGER.error("创建issue消息发送失败", e);
//        }
        MessageSender messageSender = new MessageSender();
        messageSender.setTenantId(0L);
        messageSender.setMessageCode("issueCreate");
        List<Receiver> receivers = new ArrayList<>();
        List<String> messageTypes = new ArrayList<>();
        messageTypes.add("email");
        handleReceiver(receivers,userIds,messageTypes);
        ProjectVO projectVO = baseFeignClient.queryProject(projectId).getBody();
        Map<String,String> map = new HashMap<>();
        map.put(ASSIGNEENAME, userName);
        map.put(SUMMARY, summary);
        map.put(URL, url);
        map.put(PROJECT_NAME, projectVO.getName());
        messageSender.setArgs(map);
        messageSender.setReceiverAddressList(receivers);
        //发送站内信
        messageClient.async().sendMessage(messageSender);
    }

    private void handleReceiver(List<Receiver> receivers,List<Long> userIds,List<String> messageTypes){
        List<UserDTO> users = baseFeignClient.listUsersByIds(userIds.toArray(new Long[]{}), true).getBody();
        Map<Long, UserDTO> userDTOMap = users.stream().collect(Collectors.toMap(UserDTO::getId, Function.identity()));
        for (Long userId:userIds) {
            Receiver receiver = new Receiver();
            receiver.setUserId(userId);
            UserDTO userDTO = userDTOMap.get(userId);
            if (messageTypes.contains("email")) {
                receiver.setEmail(userDTO.getEmail());
            }
            if (messageTypes.contains("phone")) {
                receiver.setPhone(userDTO.getPhone());
            }
            receivers.add(receiver);
        }
    }

    public void issueAssignee(List<Long> userIds, String userName, String summary, String url, Long assigneeId, Long projectId) {
//        NoticeSendDTO noticeSendDTO = new NoticeSendDTO();
//        // 添加普通消息
//        setCommonMsg(noticeSendDTO, projectId, "issueAssignee", userName, summary, url, userIds, assigneeId);
//        // 添加webhook
//        setWebHookJson(noticeSendDTO, noticeSendDTO.getParams(), assigneeId, "issueAssignee", "问题分配");
//        try {
//            notifyFeignClient.postNotice(noticeSendDTO);
//        } catch (Exception e) {
//            LOGGER.error("分配issue消息发送失败", e);
//        }
        MessageSender messageSender = new MessageSender();
        messageSender.setTenantId(0L);
        messageSender.setMessageCode("issueAssignee");
        List<Receiver> receivers = new ArrayList<>();
        List<String> messageTypes = new ArrayList<>();
        messageTypes.add("email");
        handleReceiver(receivers,userIds,messageTypes);
        ProjectVO projectVO = baseFeignClient.queryProject(projectId).getBody();
        Map<String,String> map = new HashMap<>();
        map.put(ASSIGNEENAME, userName);
        map.put(SUMMARY, summary);
        map.put(URL, url);
        map.put(PROJECT_NAME, projectVO.getName());
        messageSender.setArgs(map);
        messageSender.setReceiverAddressList(receivers);
        //发送站内信
        messageClient.async().sendMessage(messageSender);
    }

    public void issueSolve(List<Long> userIds, String userName, String summary, String url, Long assigneeId, Long projectId) {
//        NoticeSendDTO noticeSendDTO = new NoticeSendDTO();
//        // 添加普通消息
//        setCommonMsg(noticeSendDTO, projectId, "issueSolve", userName, summary, url, userIds, assigneeId);
//        // 添加webhook
//        setWebHookJson(noticeSendDTO, noticeSendDTO.getParams(), assigneeId, "issueSolve", "问题完成");
//        try {
//            notifyFeignClient.postNotice(noticeSendDTO);
//        } catch (Exception e) {
//            LOGGER.error("完成issue消息发送失败", e);
//        }
        MessageSender messageSender = new MessageSender();
        messageSender.setTenantId(0L);
        messageSender.setMessageCode("issueSolve");
        List<Receiver> receivers = new ArrayList<>();
        List<String> messageTypes = new ArrayList<>();
        messageTypes.add("email");
        handleReceiver(receivers,userIds,messageTypes);
        ProjectVO projectVO = baseFeignClient.queryProject(projectId).getBody();
        Map<String,String> map = new HashMap<>();
        map.put(ASSIGNEENAME, userName);
        map.put(SUMMARY, summary);
        map.put(URL, url);
        map.put(PROJECT_NAME, projectVO.getName());
        messageSender.setArgs(map);
        messageSender.setReceiverAddressList(receivers);
        //发送站内信
        messageClient.async().sendMessage(messageSender);
    }

//    private void setCommonMsg(NoticeSendDTO noticeSendDTO, Long projectId, String noticeCode, String userName,
//                              String summary, String url, List<Long> userIds, Long fromUserId) {
//        ProjectVO projectVO = baseFeignClient.queryProject(projectId).getBody();
//        noticeSendDTO.setCode(noticeCode);
//        noticeSendDTO.setNotifyType(NOTIFY_TYPE);
//        Map<String, Object> params = new HashMap<>();
//        params.put(ASSIGNEENAME, userName);
//        params.put(SUMMARY, summary);
//        params.put(URL, url);
//        params.put(PROJECT_NAME, projectVO.getName());
//        noticeSendDTO.setParams(params);
//        List<NoticeSendDTO.User> userList = new ArrayList<>();
//        for (Long id : userIds) {
//            NoticeSendDTO.User user = new NoticeSendDTO.User();
//            user.setId(id);
//            userList.add(user);
//        }
//        noticeSendDTO.setTargetUsers(userList);
//        NoticeSendDTO.User fromUser = new NoticeSendDTO.User();
//        fromUser.setId(fromUserId);
//        noticeSendDTO.setFromUser(fromUser);
//        noticeSendDTO.setSourceId(projectId);
//    }

//    private void setWebHookJson(NoticeSendDTO noticeSendDTO, Map<String, Object> params, Long userId, String objectKand, String eventName) {
//        WebHookJsonSendDTO webHookJsonSendDTO = new WebHookJsonSendDTO();
//        webHookJsonSendDTO.setObjectKind(objectKand);
//        webHookJsonSendDTO.setEventName(eventName);
//        webHookJsonSendDTO.setObjectAttributes((JSONObject) JSONObject.toJSON(params));
//        webHookJsonSendDTO.setCreatedAt(new Date());
//        webHookJsonSendDTO.setUser(userService.getWebHookUserById(userId));
//        noticeSendDTO.setWebHookJsonSendDTO(webHookJsonSendDTO);
//    }



}
