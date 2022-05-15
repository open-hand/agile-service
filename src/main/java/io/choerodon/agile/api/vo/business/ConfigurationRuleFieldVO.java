package io.choerodon.agile.api.vo.business;

import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

import java.util.List;
import java.util.Objects;

/**
 * @author superlee
 * @since 2021-08-02
 */
public class ConfigurationRuleFieldVO {

    @Encrypt
    @ApiModelProperty(value = "字段id")
    private Long fieldId;
    @ApiModelProperty(value = "配置字段值集合")
    private List<ConfigurationRuleFieldValueVO> fieldValues;

    public Long getFieldId() {
        return fieldId;
    }

    public void setFieldId(Long fieldId) {
        this.fieldId = fieldId;
    }

    public List<ConfigurationRuleFieldValueVO> getFieldValues() {
        return fieldValues;
    }

    public void setFieldValues(List<ConfigurationRuleFieldValueVO> fieldValues) {
        this.fieldValues = fieldValues;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof ConfigurationRuleFieldVO)) return false;
        ConfigurationRuleFieldVO that = (ConfigurationRuleFieldVO) o;
        return Objects.equals(getFieldId(), that.getFieldId()) &&
                Objects.equals(getFieldValues(), that.getFieldValues());
    }

    @Override
    public int hashCode() {
        return Objects.hash(getFieldId(), getFieldValues());
    }
}
