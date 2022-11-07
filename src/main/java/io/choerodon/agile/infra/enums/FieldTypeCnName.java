package io.choerodon.agile.infra.enums;

import java.util.Arrays;
import java.util.Optional;
import java.util.stream.Stream;

/**
 * 自定义字段的类型
 *
 * @author chihao.ran@hand-china.com
 * @since 2021-03-01
 */
public enum FieldTypeCnName {

    //单选框
    RADIO("单选框", "radio"),
    //复选框
    CHECKBOX("多选框", "checkbox"),
    //时间选择器
    TIME("时间选择器", "time"),
    //日期时间选择器
    DATETIME("日期时间选择器", "datetime"),
    //数字输入框
    NUMBER("数字输入框", "number"),
    //文本框（单行）
    TEXT("文本框（多行）", "text"),
    //文本框（多行）
    INPUT("文本框（单行）", "input"),
    //选择器（单选）
    SINGLE("选择器（单选）", "single"),
    //选择器（多选）
    MULTIPLE("选择器（多选）", "multiple"),
    //人员
    MEMBER("人员", "member"),
    //日期选择器
    DATE("日期选择器", "date"),
    //人员（多选）
    MULTI_MEMBER("人员（多选）", "multiMember"),
    ;
    String name;
    String code;

    FieldTypeCnName(String name, String code) {
        this.name = name;
        this.code = code;
    }

    public String getName() {
        return name;
    }

    public String getCode() {
        return code;
    }

    public static Optional<FieldTypeCnName> getFieldTypeByCnName(String name) {
        if (name == null) {
            return Optional.empty();
        }
        return Arrays.stream(FieldTypeCnName.values())
                .filter(typeCnName -> name.equals(typeCnName.getName()))
                .findFirst();
    }

    public static Optional<FieldTypeCnName> ofCode(String code) {
        if (code == null) {
            return Optional.empty();
        }
        return Arrays.stream(FieldTypeCnName.values())
                .filter(typeCnName -> code.equals(typeCnName.getCode()))
                .findFirst();
    }

    public static boolean isOption(String value) {
        return Stream.of(RADIO, CHECKBOX, SINGLE, MULTIPLE).anyMatch(
                fieldType -> fieldType.getCode().equals(value));
    }
}
