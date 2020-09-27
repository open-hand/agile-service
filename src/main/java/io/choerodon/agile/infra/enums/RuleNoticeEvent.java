package io.choerodon.agile.infra.enums;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;

import io.choerodon.agile.infra.utils.SendMsgUtil;
import org.hzero.boot.message.entity.MessageSender;

/**
 * 页面规则通知事件
 * @author jiaxu.cui@hand-china.com 2020/9/27 下午1:56
 */
public enum RuleNoticeEvent {
    
    ISSUE_CREATED(new String[]{"ISSUECREATE","ISSUEASSIGNEE"}),
    ISSUE_ASIGNEED(new String[]{"ISSUEASSIGNEE"}),
    ISSUE_RESOLVED(new String[]{"ISSUESOLVE"});
    
    private String[] messageCodeList;

    RuleNoticeEvent(String[] messageCodeList) {
        this.messageCodeList = messageCodeList;
    }

    public String[] getMessageCodeList() {
        return messageCodeList;
    }
}
