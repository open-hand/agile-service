package io.choerodon.agile.api.vo.search;

import io.swagger.annotations.ApiModelProperty;

import org.hzero.starter.keyencrypt.core.Encrypt;

/**
 * @author superlee
 * @since 2022-11-02
 */
public class Field {
    @ApiModelProperty("字段编码，系统字段使用")
    private String fieldCode;
    @ApiModelProperty("字段编码，自定义字段使用，解决多项目code重复问题")
    @Encrypt
    private Long fieldId;
    @ApiModelProperty("字段类型")
    private String fieldType;
    @ApiModelProperty("是否为预定义字段")
    private Boolean predefined;

    private Boolean noEncryptFlag;

    public Boolean getNoEncryptFlag() {
        return noEncryptFlag;
    }

    public void setNoEncryptFlag(Boolean noEncryptFlag) {
        this.noEncryptFlag = noEncryptFlag;
    }

    public Long getFieldId() {
        return fieldId;
    }

    public void setFieldId(Long fieldId) {
        this.fieldId = fieldId;
    }

    public String getFieldCode() {
        return fieldCode;
    }

    public void setFieldCode(String fieldCode) {
        this.fieldCode = fieldCode;
    }

    public String getFieldType() {
        return fieldType;
    }

    public void setFieldType(String fieldType) {
        this.fieldType = fieldType;
    }

    public Boolean getPredefined() {
        return predefined;
    }

    public void setPredefined(Boolean predefined) {
        this.predefined = predefined;
    }
}
