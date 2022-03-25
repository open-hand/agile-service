package io.choerodon.agile.api.vo;


import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

/**
 * @author shinan.chen
 * @since 2019/4/8
 */
public class PageFieldViewCreateVO {
    @ApiModelProperty(value = "字段id")
    @Encrypt
    private Long fieldId;
    @ApiModelProperty(value = "字段类型")
    private String fieldType;
    @ApiModelProperty(value = "字段值")
    private Object value;
    private String fieldCode;

    public String getFieldCode() {
        return fieldCode;
    }

    public void setFieldCode(String fieldCode) {
        this.fieldCode = fieldCode;
    }

    public Long getFieldId() {
        return fieldId;
    }

    public void setFieldId(Long fieldId) {
        this.fieldId = fieldId;
    }

    public String getFieldType() {
        return fieldType;
    }

    public void setFieldType(String fieldType) {
        this.fieldType = fieldType;
    }

    public Object getValue() {
        return value;
    }

    public void setValue(Object value) {
        this.value = value;
    }

    @Override
    public String toString() {
        return "PageFieldViewCreateVO{" +
                "fieldId=" + fieldId +
                ", fieldType='" + fieldType + '\'' +
                ", value=" + value +
                ", fieldCode='" + fieldCode + '\'' +
                '}';
    }
}
