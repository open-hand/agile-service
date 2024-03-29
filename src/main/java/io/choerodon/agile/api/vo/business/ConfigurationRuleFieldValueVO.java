package io.choerodon.agile.api.vo.business;

import org.hzero.starter.keyencrypt.core.Encrypt;

import java.math.BigDecimal;
import java.util.Date;
import java.util.Objects;

/**
 * @author superlee
 * @since 2021-08-05
 */
public class ConfigurationRuleFieldValueVO {

    @Encrypt
    private Long id;
    @Encrypt
    private Long ruleId;
    @Encrypt
    private Long ruleFieldId;

    private String fieldType;

    private String operateType;
    @Encrypt
    private Long optionId;

    private String stringValue;

    private BigDecimal numberValue;

    private BigDecimal numberAddValue;

    private String textValue;

    private Date dateValue;

    private BigDecimal dateAddValue;
    @Encrypt
    private Long userId;
    @Encrypt
    private Long customFieldId;

    private Long projectId;

    private Long organizationId;

    public Long getCustomFieldId() {
        return customFieldId;
    }

    public void setCustomFieldId(Long customFieldId) {
        this.customFieldId = customFieldId;
    }

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public Long getRuleId() {
        return ruleId;
    }

    public void setRuleId(Long ruleId) {
        this.ruleId = ruleId;
    }

    public Long getRuleFieldId() {
        return ruleFieldId;
    }

    public void setRuleFieldId(Long ruleFieldId) {
        this.ruleFieldId = ruleFieldId;
    }

    public String getFieldType() {
        return fieldType;
    }

    public void setFieldType(String fieldType) {
        this.fieldType = fieldType;
    }

    public String getOperateType() {
        return operateType;
    }

    public void setOperateType(String operateType) {
        this.operateType = operateType;
    }

    public Long getOptionId() {
        return optionId;
    }

    public void setOptionId(Long optionId) {
        this.optionId = optionId;
    }

    public String getStringValue() {
        return stringValue;
    }

    public void setStringValue(String stringValue) {
        this.stringValue = stringValue;
    }

    public BigDecimal getNumberValue() {
        return numberValue;
    }

    public void setNumberValue(BigDecimal numberValue) {
        this.numberValue = numberValue;
    }

    public BigDecimal getNumberAddValue() {
        return numberAddValue;
    }

    public void setNumberAddValue(BigDecimal numberAddValue) {
        this.numberAddValue = numberAddValue;
    }

    public String getTextValue() {
        return textValue;
    }

    public void setTextValue(String textValue) {
        this.textValue = textValue;
    }

    public Date getDateValue() {
        return dateValue;
    }

    public void setDateValue(Date dateValue) {
        this.dateValue = dateValue;
    }

    public BigDecimal getDateAddValue() {
        return dateAddValue;
    }

    public void setDateAddValue(BigDecimal dateAddValue) {
        this.dateAddValue = dateAddValue;
    }

    public Long getUserId() {
        return userId;
    }

    public void setUserId(Long userId) {
        this.userId = userId;
    }

    public Long getProjectId() {
        return projectId;
    }

    public void setProjectId(Long projectId) {
        this.projectId = projectId;
    }

    public Long getOrganizationId() {
        return organizationId;
    }

    public void setOrganizationId(Long organizationId) {
        this.organizationId = organizationId;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof ConfigurationRuleFieldValueVO)) return false;
        ConfigurationRuleFieldValueVO that = (ConfigurationRuleFieldValueVO) o;
        return Objects.equals(getFieldType(), that.getFieldType()) &&
                Objects.equals(getOperateType(), that.getOperateType()) &&
                Objects.equals(getOptionId(), that.getOptionId()) &&
                Objects.equals(getStringValue(), that.getStringValue()) &&
                Objects.equals(getNumberValue(), that.getNumberValue()) &&
                Objects.equals(getNumberAddValue(), that.getNumberAddValue()) &&
                Objects.equals(getTextValue(), that.getTextValue()) &&
                Objects.equals(getDateValue(), that.getDateValue()) &&
                Objects.equals(getDateAddValue(), that.getDateAddValue()) &&
                Objects.equals(getUserId(), that.getUserId()) &&
                Objects.equals(getCustomFieldId(), that.getCustomFieldId());
    }

    @Override
    public int hashCode() {
        return Objects.hash(getFieldType(), getOperateType(), getOptionId(), getStringValue(), getNumberValue(), getNumberAddValue(), getTextValue(), getDateValue(), getDateAddValue(), getUserId(), getCustomFieldId());
    }
}
