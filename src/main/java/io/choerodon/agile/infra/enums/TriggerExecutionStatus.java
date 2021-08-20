package io.choerodon.agile.infra.enums;

/**
 * @author zhaotianxin
 * @date 2021-08-20 14:22
 */
public enum TriggerExecutionStatus {

    SUCCESS("success"),
    STOP("stop"),
    LOOP("loop"),
    MAX_DEPTH("max_depth"),
    ERROR("error");

    private String value;

    TriggerExecutionStatus(String value) {
        this.value = value;
    }

    public String getValue() {
        return value;
    }

    public void setValue(String value) {
        this.value = value;
    }
}
