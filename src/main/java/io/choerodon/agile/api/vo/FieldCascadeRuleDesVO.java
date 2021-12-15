package io.choerodon.agile.api.vo;

import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

import java.util.Objects;

/**
 * @author chihao.ran@hand-china.com
 * 2021/07/20 16:00
 */
public class FieldCascadeRuleDesVO {
    @ApiModelProperty(value = "字段id")
    @Encrypt
    private Long fieldId;
    @ApiModelProperty(value = "字段编码")
    private String fieldCode;
    @ApiModelProperty(value = "字段名称")
    private String fieldName;
    @ApiModelProperty(value = "级联字段id")
    @Encrypt
    private Long cascadeFieldId;
    @ApiModelProperty(value = "级联字段名称")
    private String cascadeFieldName;
    @ApiModelProperty(value = "级联字段编码")
    private String cascadeFieldCode;

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

    public String getFieldName() {
        return fieldName;
    }

    public void setFieldName(String fieldName) {
        this.fieldName = fieldName;
    }

    public Long getCascadeFieldId() {
        return cascadeFieldId;
    }

    public void setCascadeFieldId(Long cascadeFieldId) {
        this.cascadeFieldId = cascadeFieldId;
    }

    public String getCascadeFieldName() {
        return cascadeFieldName;
    }

    public void setCascadeFieldName(String cascadeFieldName) {
        this.cascadeFieldName = cascadeFieldName;
    }

    public String getCascadeFieldCode() {
        return cascadeFieldCode;
    }

    public void setCascadeFieldCode(String cascadeFieldCode) {
        this.cascadeFieldCode = cascadeFieldCode;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof FieldCascadeRuleDesVO)) return false;
        FieldCascadeRuleDesVO that = (FieldCascadeRuleDesVO) o;
        return Objects.equals(getFieldId(), that.getFieldId()) &&
                Objects.equals(getCascadeFieldId(), that.getCascadeFieldId());
    }

    @Override
    public int hashCode() {
        return Objects.hash(getFieldId(), getCascadeFieldId());
    }
}
