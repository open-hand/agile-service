package io.choerodon.agile.infra.feign.fallback;

import io.choerodon.agile.api.vo.ProjectMessageVO;
import io.choerodon.agile.infra.feign.NotifyFeignClient;
import io.choerodon.agile.infra.feign.vo.MessageSettingVO;
import io.choerodon.core.exception.CommonException;

import org.hzero.boot.message.entity.MessageSender;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Component;

import java.util.List;


@Component
public class NotifyFeignClientFallback implements NotifyFeignClient {

    private static final String GET_MESSAGE = "notify.get.message.setting.error";

    @Override
    public ResponseEntity<MessageSettingVO> getMessageSetting(Long projectId, String notifyType, String code, Long envId, String eventName) {
        throw new CommonException(GET_MESSAGE);
    }

    @Override
    public ResponseEntity<String> checkLog(String version, String type) {
        throw new CommonException("error.data.Miration");
    }

    @Override
    public ResponseEntity<List<ProjectMessageVO>> listEnabledSettingByCode(String code, String notifyType) {
        throw new CommonException("error.data.listEnabledSettingByCode");
    }

    @Override
    public ResponseEntity<Void> batchSendMessage(List<MessageSender> senderList) {
        throw new CommonException("error.batch.sendMessage");
    }
}
