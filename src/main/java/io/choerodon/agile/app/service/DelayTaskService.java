package io.choerodon.agile.app.service;

import io.choerodon.agile.api.vo.ProjectMessageVO;
import io.choerodon.agile.infra.dto.UserDTO;
import org.hzero.boot.message.entity.MessageSender;

import java.util.List;
import java.util.Map;

/**
 * @author superlee
 * @since 2021-03-08
 */
public interface DelayTaskService {


    Map<Long, ProjectMessageVO> listEnabledMsgProjects(String msgCode);

    MessageSender buildSender(Long tenantId,
                              String messageCode,
                              Map<String, String> paramMap,
                              List<UserDTO> users);

    void batchSendMessage(List<MessageSender> messageSenders, int step);
}
