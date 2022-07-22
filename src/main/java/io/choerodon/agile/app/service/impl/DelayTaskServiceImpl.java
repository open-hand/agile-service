package io.choerodon.agile.app.service.impl;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

import io.choerodon.agile.api.vo.ProjectMessageVO;
import io.choerodon.agile.app.service.DelayTaskService;
import io.choerodon.agile.infra.dto.UserDTO;
import io.choerodon.agile.infra.feign.operator.NotifyClientOperator;
import org.hzero.boot.message.entity.MessageSender;
import org.hzero.boot.message.entity.Receiver;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.ObjectUtils;

/**
 * @author superlee
 * @since 2021-03-08
 */
@Service
public class DelayTaskServiceImpl implements DelayTaskService {

    @Autowired
    private NotifyClientOperator notifyClientOperator;

    @Override
    public Map<Long, ProjectMessageVO> listEnabledMsgProjects(String msgCode) {
        //获取所有配置过冲刺延期发送消息的项目
        List<ProjectMessageVO> projectMessageList =
                notifyClientOperator.listEnabledSettingByCode(msgCode, "agile");
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
        if (!ObjectUtils.isEmpty(messageSenders)) {
            for (int i = 0; i < messageSenders.size(); i += step) {
                int end = i + step;
                if (end >= messageSenders.size()) {
                    end = messageSenders.size();
                }
                List<MessageSender> messageSenderList = messageSenders.subList(i, end);
                notifyClientOperator.batchSendMessage(messageSenderList);
            }
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
