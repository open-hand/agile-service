package io.choerodon.agile.infra.enums;

/**
 * @author superlee
 * @since 2023/6/2
 */
public enum DataLogType {

    /**
     * agile_data_log
     */
    ISSUE("issue"),
    /**
     * backlog_data_log
     */
    BACKLOG("backlog"),
    /**
     * fd_field_data_log
     */
    CUSTOM_FIELD("custom_field"),
    ;

    private String value;

    DataLogType(String value) {
        this.value = value;
    }

    public String value() {
        return value;
    }
}
