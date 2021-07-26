package io.choerodon.agile.app.service;

import io.choerodon.agile.api.vo.business.IssueVO;
import io.choerodon.agile.api.vo.MessageVO;
import io.choerodon.agile.infra.dto.MessageDetailDTO;

import java.util.List;

/**
 * Created by HuangFuqiang@choerodon.io on 2018/10/9.
 * Email: fuqianghuang01@gmail.com
 */
public interface NoticeService {

    List<MessageVO> queryByProjectId(Long projectId);

    void updateNotice(Long projectId, List<MessageVO> messageVOList);

    List<Long> queryUserIdsByProjectId(Long projectId, String event, IssueVO issueVO);

    List<MessageDetailDTO> migrateMessageDetail();

    List<Long> queryCustomFieldUserIdsByProjectId(Long projectId, String event, IssueVO issueVO);
}
