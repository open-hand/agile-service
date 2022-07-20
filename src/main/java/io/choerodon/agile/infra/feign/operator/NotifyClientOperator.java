package io.choerodon.agile.infra.feign.operator;

import java.util.List;

import com.fasterxml.jackson.core.type.TypeReference;
import io.choerodon.agile.api.vo.ProjectMessageVO;
import io.choerodon.agile.infra.feign.NotifyFeignClient;
import io.choerodon.agile.infra.feign.vo.MessageSettingVO;
import org.hzero.boot.message.entity.MessageSender;
import org.hzero.core.util.ResponseUtils;
import org.springframework.stereotype.Component;

@Component
public class NotifyClientOperator {

    private final NotifyFeignClient notifyFeignClient;

    public NotifyClientOperator(NotifyFeignClient notifyFeignClient) {
        this.notifyFeignClient = notifyFeignClient;
    }

    public MessageSettingVO getMessageSetting(Long projectId,
                                              String notifyType,
                                              String code,
                                              Long envId,
                                              String eventName) {
        return ResponseUtils.getResponse(notifyFeignClient.getMessageSetting(projectId, notifyType, code, envId, eventName), MessageSettingVO.class);
    }

    public String checkLog(String version, String type) {
        return ResponseUtils.getResponse(notifyFeignClient.checkLog(version, type), String.class);
    }

    public List<ProjectMessageVO> listEnabledSettingByCode(String code, String notifyType) {
        return ResponseUtils.getResponse(notifyFeignClient.listEnabledSettingByCode(code, notifyType),
                new TypeReference<List<ProjectMessageVO>>() {
                });
    }

    public void batchSendMessage(List<MessageSender> senderList) {
        ResponseUtils.getResponse(notifyFeignClient.batchSendMessage(senderList), Void.class);
    }
}
