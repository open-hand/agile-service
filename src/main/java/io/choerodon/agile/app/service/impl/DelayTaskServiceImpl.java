package io.choerodon.agile.app.service.impl;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.ListUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.ObjectUtils;

import io.choerodon.agile.api.vo.ProjectMessageVO;
import io.choerodon.agile.app.service.DelayTaskService;
import io.choerodon.agile.infra.dto.UserDTO;
import io.choerodon.agile.infra.feign.NotifyFeignClient;

import org.hzero.boot.message.entity.MessageSender;
import org.hzero.boot.message.entity.Receiver;

/**
 * @author superlee
 * @since 2021-03-08
 */
@Service
public class DelayTaskServiceImpl implements DelayTaskService {

    @Autowired
    private NotifyFeignClient notifyFeignClient;

    @Override
    public Map<Long, ProjectMessageVO> listEnabledMsgProjects(String msgCode) {
        //获取所有配置过冲刺延期发送消息的项目
        List<ProjectMessageVO> projectMessageList =
                notifyFeignClient.listEnabledSettingByCode(msgCode, "agile").getBody();
        if (ObjectUtils.isEmpty(projectMessageList)) {
            return new HashMap<>();
        } else {
            return projectMessageList
                    .stream()
                    .collect(Collectors.toMap(ProjectMessageVO::getId, Function.identity()));
        }
    }

    @Override
    public MessageSender buildSender(Long tenantId, String messageCode, Map<String, String> paramMap, List<UserDTO> users) {
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

    @Override
    public void batchSendMessage(List<MessageSender> messageSenders, int step) {
        if(CollectionUtils.isEmpty(messageSenders) || step < 1) {
            return;
        }
        for (List<MessageSender> partitionSenders : ListUtils.partition(messageSenders, step)) {
            notifyFeignClient.batchSendMessage(partitionSenders);
        }
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
