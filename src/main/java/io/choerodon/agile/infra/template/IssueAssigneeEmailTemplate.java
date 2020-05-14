//package io.choerodon.agile.infra.template;
//
//import io.choerodon.core.exception.CommonException;
//import org.springframework.stereotype.Component;
//
//import java.io.IOException;
//
///**
// * User: Mr.Wang
// * Date: 2019/10/28
// */
//@Component
//public class IssueAssigneeEmailTemplate implements DefaultEmailTemplate {
//
//    @Override
//    public String businessTypeCode() {
//        return "issueAssignee";
//    }
//
//    @Override
//    public String code() {
//        return "issueAssignee-preset";
//    }
//
//    @Override
//    public String name() {
//        return "Choerodon通知-问题分配";
//    }
//
//    @Override
//    public String title() {
//        return "Choerodon通知-问题分配";
//    }
//
//
//    @Override
//    public String content() {
//        String content;
//        try {
//            content = content("/templates/IssueAssigneeTemplate.html");
//        } catch (IOException e) {
//            throw new CommonException(e);
//        }
//        return content;
//    }
//}
