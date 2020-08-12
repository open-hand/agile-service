package io.choerodon.agile.infra.enums;

/**
 * @author jiaxu.cui@hand-china.com 2020/8/12 下午2:59
 */
public enum StatusNoticeUserType {
    PROJECT_OWNER("projectOwner"),
    ASSIGNEE("assignee"),
    REPORTER("reporter"),
    SPECIFIER("specifier");

    private String code;

    StatusNoticeUserType(String code) {
        this.code = code;
    }

    public String getCode() {
        return code;
    }
}
