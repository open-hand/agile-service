package io.choerodon.agile.infra.enums;

/**
 * @author jiaxu.cui@hand-china.com 2020/8/12 下午2:59
 */
public class StatusNoticeUserType {

    public static final String PROJECT_OWNER = "projectOwner";
    public static final String ASSIGNEE = "assignee";
    public static final String REPORTER = "reporter";
    public static final String SPECIFIER = "specifier";
    public static final String ONLY_WEB_HOOK = "";

    public static final String[] BASE_USER_TYPE_LIST = new String[]{PROJECT_OWNER, ASSIGNEE, REPORTER};
}
