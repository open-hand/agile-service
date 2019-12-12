package io.choerodon.agile.infra.template;


import io.choerodon.core.notify.*;
import org.springframework.stereotype.Component;

/**
 * Created by HuangFuqiang@choerodon.io on 2018/10/9.
 * Email: fuqianghuang01@gmail.com
 */
@Component
@NotifyBusinessType(code = "issueSolve",
        name = "问题已解决",
        description = "问题已解决，给相关用户发送通知",
        level = Level.PROJECT,
        categoryCode = "issue-status-change-notice",
        pmEnabledFlag = true,
        proPmEnabledFlag = true,
        notifyType = ServiceNotifyType.AGILE_NOTIFY,
        targetUserType = {TargetUserType.TARGET_USER_ASSIGNEE, TargetUserType.TARGET_USER_REPORTER})
public class IssueSolvePmTemplate implements PmTemplate {

    @Override
    public String businessTypeCode() {
        return "issueSolve";
    }

    @Override
    public String code() {
        return "issueSolve-preset";
    }

    @Override
    public String name() {
        return "问题已解决";
    }

    @Override
    public String title() {
        return "问题已解决";
    }

    @Override
    public String content() {
        return "<p><a href=${url} target=_blank>${summary}</a> 已经由 ${assigneeName} 解决</p>";
    }
}
