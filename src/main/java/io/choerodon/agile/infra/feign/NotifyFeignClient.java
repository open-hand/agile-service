package io.choerodon.agile.infra.feign;

import java.util.List;

import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import io.choerodon.agile.infra.feign.fallback.NotifyFallbackFactory;

import org.hzero.boot.message.entity.MessageSender;
import org.hzero.common.HZeroService;

@FeignClient(value = HZeroService.Message.NAME, fallbackFactory = NotifyFallbackFactory.class)
public interface NotifyFeignClient {


    @GetMapping("/choerodon/v1/projects/{project_id}/message_settings/type/{notify_type}/code/{code}")
    ResponseEntity<String> getMessageSetting(@PathVariable(value = "project_id") Long projectId,
                                             @PathVariable(value = "notify_type") String notifyType,
                                             @PathVariable(value = "code") String code,
                                             @RequestParam(value = "env_id", required = false) Long envId,
                                             @RequestParam(value = "event_name", required = false) String eventName);

    @GetMapping("/choerodon/v1/upgrade")
    ResponseEntity<String> checkLog(@RequestParam(value = "version") String version,
                                    @RequestParam(value = "type") String type);

    @PostMapping("/choerodon/v1/messages/setting/{code}/enabled")
    ResponseEntity<String> listEnabledSettingByCode(@PathVariable String code,
                                                    @RequestParam(value = "notify_type") String notifyType);

    @PostMapping("/choerodon/v1/messages/send/batch")
    ResponseEntity<String> batchSendMessage(@RequestBody List<MessageSender> senderList);
}
