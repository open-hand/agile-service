package io.choerodon.agile.api.vo;

import org.hzero.starter.keyencrypt.core.Encrypt;

import java.util.List;

/**
 * @author superlee
 * @since 2021-08-02
 */
public class ConfigurationRuleFieldVO {

    @Encrypt
    private Long fieldId;

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
}
