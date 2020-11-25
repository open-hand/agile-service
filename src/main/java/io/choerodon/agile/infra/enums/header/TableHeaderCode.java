package io.choerodon.agile.infra.enums.header;


import java.util.ArrayList;
import java.util.List;

/**
 * @author superlee
 * @since 2020-11-25
 */
public enum TableHeaderCode {

    GANTT_CHART("gantt_chart");

    private String code;

    private static final List<String> CODES;

    TableHeaderCode(String code) {
        this.code = code;
    }

    public String code() {
        return this.code;
    }

    static {
        CODES = new ArrayList<>();
        for (TableHeaderCode tableHeaderCode : TableHeaderCode.values()) {
            CODES.add(tableHeaderCode.code());
        }
    }

    public static boolean notExisted(String code) {
        return CODES.contains(code);
    }
}
