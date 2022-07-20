package io.choerodon.agile.infra.feign;

import java.util.List;

import io.choerodon.agile.api.vo.ProjectMessageVO;
import io.choerodon.agile.infra.feign.fallback.NotifyFallbackFactory;
import io.choerodon.agile.infra.feign.vo.MessageSettingVO;
import org.hzero.boot.message.entity.MessageSender;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@FeignClient(value = "choerodon-message", fallbackFactory = NotifyFallbackFactory.class)
public interface NotifyFeignClient {


    @GetMapping("/choerodon/v1/projects/{project_id}/message_settings/type/{notify_type}/code/{code}")
    ResponseEntity<MessageSettingVO> getMessageSetting(@PathVariable(value = "project_id") Long projectId,
                                                       @PathVariable(value = "notify_type") String notifyType,
                                                       @PathVariable(value = "code") String code,
                                                       @RequestParam(value = "env_id", required = false) Long envId,
                                                       @RequestParam(value = "event_name", required = false) String eventName);

    @GetMapping("/choerodon/v1/upgrade")
    ResponseEntity<String> checkLog(@RequestParam(value = "version") String version,
                                    @RequestParam(value = "type") String type);

    @PostMapping("/choerodon/v1/messages/setting/{code}/enabled")
    ResponseEntity<List<ProjectMessageVO>> listEnabledSettingByCode(@PathVariable String code,
                                                                    @RequestParam(value = "notify_type") String notifyType);

    @PostMapping("/choerodon/v1/messages/send/batch")
    ResponseEntity<Void> batchSendMessage(@RequestBody List<MessageSender> senderList);
}
