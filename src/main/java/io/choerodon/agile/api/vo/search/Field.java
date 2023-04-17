package io.choerodon.agile.api.vo.search;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.annotations.ApiModelProperty;

import org.hzero.starter.keyencrypt.core.Encrypt;

/**
 * @author superlee
 * @since 2022-11-02
 */
@JsonInclude(JsonInclude.Include.NON_NULL)
public class Field implements Serializable {
    @ApiModelProperty("字段编码，系统字段使用")
    private String fieldCode;
    @ApiModelProperty("字段编码，自定义字段使用，解决多项目code重复问题")
    @Encrypt
    private Long fieldId;
    @ApiModelProperty("字段类型")
    private String fieldType;
    @ApiModelProperty("是否为预定义字段")
    private Boolean predefined;
    @ApiModelProperty("字段名称，前端个人筛选保存使用")
    private String name;
    @ApiModelProperty("不加密标记")
    private Boolean noEncryptFlag;

    public String getName() {
        return name;
    }

    public Field setName(String name) {
        this.name = name;
        return this;
    }

    public Boolean getNoEncryptFlag() {
        return noEncryptFlag;
    }

    public Field setNoEncryptFlag(Boolean noEncryptFlag) {
        this.noEncryptFlag = noEncryptFlag;
        return this;
    }

    public Long getFieldId() {
        return fieldId;
    }

    public Field setFieldId(Long fieldId) {
        this.fieldId = fieldId;
        return this;
    }

    public String getFieldCode() {
        return fieldCode;
    }

    public Field setFieldCode(String fieldCode) {
        this.fieldCode = fieldCode;
        return this;
    }

    public String getFieldType() {
        return fieldType;
    }

    public Field setFieldType(String fieldType) {
        this.fieldType = fieldType;
        return this;
    }

    public Boolean getPredefined() {
        return predefined;
    }

    public Field setPredefined(Boolean predefined) {
        this.predefined = predefined;
        return this;
    }

    @Override
    public String toString() {
        return "Field{" +
                "fieldCode='" + fieldCode + '\'' +
                ", fieldId=" + fieldId +
                ", fieldType='" + fieldType + '\'' +
                ", predefined=" + predefined +
                ", noEncryptFlag=" + noEncryptFlag +
                '}';
    }
}
