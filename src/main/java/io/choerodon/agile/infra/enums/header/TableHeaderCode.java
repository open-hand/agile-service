package io.choerodon.agile.infra.enums.header;

import io.choerodon.core.exception.CommonException;

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

    public static boolean notExisted(String code, boolean throwException) {
        boolean contains = CODES.contains(code);
        if (!contains && throwException) {
            throw new CommonException("error.illegal.table.header.code." + code);
        }
        return contains;
    }
}
