package io.choerodon.agile.api.vo.search;

import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.annotations.ApiModelProperty;

import org.hzero.starter.keyencrypt.core.Encrypt;

/**
 * @author superlee
 * @since 2022-11-02
 */
@JsonInclude(JsonInclude.Include.NON_NULL)
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
    @ApiModelProperty("字段名称，前端个人筛选保存使用")
    private String name;
    @ApiModelProperty("不加密标记")
    private Boolean noEncryptFlag;

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

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
