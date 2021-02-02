package io.choerodon.agile.infra.enums;

/**
 * 自定义字段的类型
 *
 * @author superlee
 * @since 2020-05-07
 */
public enum FieldTypeCnName {
    //文本框（单行）
    TEXT("文本框（单行）"),
    //单选框
    RADIO("单选框"),
    //复选框
    CHECKBOX("复选框"),
    //时间选择器
    TIME("时间选择器"),
    //日期时间选择器
    DATETIME("日期时间选择器"),
    //数字输入框
    NUMBER("数字输入框"),
    //文本框（多行）
    INPUT("文本框（多行）"),
    //选择器（单选）
    SINGLE("选择器（单选）"),
    //选择器（多选）
    MULTIPLE("选择器（多选）"),
    //人员
    MEMBER("人员"),
    //日期选择器
    DATE("日期选择器"),
    //人员（多选）
    MULTI_MEMBER("人员（多选）"),
    ;
    String name;

    FieldTypeCnName(String name) {
        this.name = name;
    }

    public String getName() {
        return name;
    }
}
