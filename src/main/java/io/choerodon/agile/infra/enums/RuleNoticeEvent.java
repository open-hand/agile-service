package io.choerodon.agile.infra.enums;

/**
 * 页面规则通知事件
 * @author jiaxu.cui@hand-china.com 2020/9/27 下午1:56
 */
public class RuleNoticeEvent {

    /**
     * 事件代码
     */
    public static final String ISSUE_CREATED = "ISSUE_CREATED";
    public static final String ISSUE_UPDATE = "ISSUE_UPDATE";
    public static final String ISSUE_RESOLVED = "ISSUE_RESOLVED";
    public static final String ISSUE_STATAUS_CHANGE = "ISSUE_STATAUS_CHANGE";

    /**
     * 消息代码
     */
    public static final String ISSUECREATE = "ISSUECREATE";
    public static final String ISSUEASSIGNEE = "ISSUEASSIGNEE";
    public static final String ISSUESOLVE = "ISSUESOLVE";
    public static final String ISSUECHANGESTATUS = "ISSUECHANGESTATUS";
    public static final String AUTO_RULE_TRIGGER = "AUTO_RULE_TRIGGER";

    public static String[] getMsgCode(String event){
        String[] msgCode = null;
        switch (event){
            case ISSUE_CREATED:
                msgCode = new String[]{ISSUECREATE,ISSUEASSIGNEE};
                break;
            case ISSUE_UPDATE:
                msgCode = new String[]{ISSUEASSIGNEE,ISSUESOLVE, ISSUECHANGESTATUS};
                break;
            case ISSUE_RESOLVED:
                msgCode = new String[]{ISSUESOLVE};
                break;
            case ISSUE_STATAUS_CHANGE:
                msgCode = new String[]{ISSUESOLVE,ISSUECHANGESTATUS};
                break;
            default:
                break;
        }
        return msgCode;
    }
}
