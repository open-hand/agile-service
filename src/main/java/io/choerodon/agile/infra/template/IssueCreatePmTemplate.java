//package io.choerodon.agile.infra.template;
//
//import io.choerodon.core.notify.*;
//import org.springframework.stereotype.Component;
//
///**
// * Created by HuangFuqiang@choerodon.io on 2018/10/9.
// * Email: fuqianghuang01@gmail.com
// */
//@Component
//@NotifyBusinessType(code = "issueCreate",
//        name = "问题创建",
//        description = "问题创建，给相关用户发送通知",
//        level = Level.PROJECT,
//        categoryCode = "issue-status-change-notice",
//        pmEnabledFlag = true,
//        proPmEnabledFlag = true,
//        emailEnabledFlag = true,
//        notifyType = ServiceNotifyType.AGILE_NOTIFY,
//        targetUserType = {TargetUserType.TARGET_USER_ASSIGNEE})
//public class IssueCreatePmTemplate implements PmTemplate {
//
//    @Override
//    public String businessTypeCode() {
//        return "issueCreate";
//    }
//
//    @Override
//    public String code() {
//        return "issueCreate-preset";
//    }
//
//    @Override
//    public String name() {
//        return "问题创建";
//    }
//
//    @Override
//    public String title() {
//        return "问题创建";
//    }
//
//    @Override
//    public String content() {
//        return "<p>${assigneeName} 创建了问题 <a href=${url} target=_blank>${summary}</a ></p>";
//    }
//}
