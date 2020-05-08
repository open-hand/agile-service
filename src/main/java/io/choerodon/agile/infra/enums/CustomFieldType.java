package io.choerodon.agile.infra.enums;

import io.choerodon.core.exception.CommonException;

/**
 * 自定义字段的类型
 *
 * @author superlee
 * @since 2020-05-07
 */
public enum CustomFieldType {
    /**
     * 单选框，复选框，选择器(单选)，选择器(多选)，人员
     */
    OPTION,

    /**
     * 日期时间选择器，日期选择器
     */
    DATE,

    /**
     * 时间选择器，
     */
    DATE_HMS,

    /**
     * 数字输入框
     */
    NUMBER,

    /**
     * 单行文本框，
     */
    STRING,

    /**
     * 多行文本框
     */
    TEXT;

    public static boolean isOption(String value) {
        return OPTION.name().equalsIgnoreCase(value);
    }

    public static boolean isDate(String value) {
        return DATE.name().equalsIgnoreCase(value);
    }

    public static boolean isDateHms(String value) {
        return DATE_HMS.name().equalsIgnoreCase(value);
    }

    public static boolean isNumber(String value) {
        return NUMBER.name().equalsIgnoreCase(value);
    }

    public static boolean isString(String value) {
        return STRING.name().equalsIgnoreCase(value);
    }

    public static boolean isText(String value) {
        return TEXT.name().equalsIgnoreCase(value);
    }


    public static boolean contains(String value, boolean thrownException) {
        boolean contains = false;
        for (CustomFieldType type : CustomFieldType.values()) {
            if (type.name().equalsIgnoreCase(value)) {
                contains = true;
                break;
            }
        }
        if (!contains && thrownException) {
            throw new CommonException("error.illegal.custom.field.type.[" + value + "].not.in." + valuesToString());
        }
        return contains;
    }

    public static String valuesToString() {
        StringBuilder builder = new StringBuilder("[");
        for (CustomFieldType type : CustomFieldType.values()) {
            builder.append(type.name().toLowerCase()).append(",");
        }
        builder.append("]");
        return builder.toString();
    }

}
