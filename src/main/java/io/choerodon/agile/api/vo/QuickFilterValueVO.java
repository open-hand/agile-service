package io.choerodon.agile.api.vo;

import io.swagger.annotations.ApiModelProperty;

/**
 * Created by HuangFuqiang@choerodon.io on 2018/6/14.
 * Email: fuqianghuang01@gmail.com
 */
public class QuickFilterValueVO {

    @ApiModelProperty(value = "快速搜索字段编码")
    private String fieldCode;

    @ApiModelProperty(value = "快速搜索表达式操作关系：and、or等")
    private String operation;

    @ApiModelProperty(value = "快速搜索值")
    private String value;

    /**
     * 是否为预定义字段，必填
     */
    @ApiModelProperty(value = "是否为预定义字段")
    private Boolean predefined;

    /**
     * 自定义字段的字段类型
     */
    @ApiModelProperty(value = "自定义字段的字段类型")
    private String customFieldType;

    public String getCustomFieldType() {
        return customFieldType;
    }

    public void setCustomFieldType(String customFieldType) {
        this.customFieldType = customFieldType;
    }

    public Boolean getPredefined() {
        return predefined;
    }

    public void setPredefined(Boolean predefined) {
        this.predefined = predefined;
    }

    public void setFieldCode(String fieldCode) {
        this.fieldCode = fieldCode;
    }

    public String getFieldCode() {
        return fieldCode;
    }

    public String getOperation() {
        return operation;
    }

    public void setOperation(String operation) {
        this.operation = operation;
    }

    public String getValue() {
        return value;
    }

    public void setValue(String value) {
        this.value = value;
    }
}
