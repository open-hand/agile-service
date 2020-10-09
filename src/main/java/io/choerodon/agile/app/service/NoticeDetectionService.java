package io.choerodon.agile.app.service;

import java.util.List;

import io.choerodon.agile.infra.enums.RuleNoticeEvent;

/**
 * @author jiaxu.cui@hand-china.com 2020/10/9 上午9:59
 */
public interface NoticeDetectionService {

    /**
     * issue通知检测
     * @param event 通知事件
     * @param issueId issueId
     * @param projectId projectId
     * @param fieldList 若是批量更改，需要确认更改的字段是否触发消息
     */
    void issueNoticeDetection(RuleNoticeEvent event, Long issueId, Long projectId, List<String> fieldList);
}
