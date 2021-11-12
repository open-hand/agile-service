package io.choerodon.agile.infra.dto;

import io.choerodon.mybatis.annotation.ModifyAudit;
import io.choerodon.mybatis.annotation.VersionAudit;
import io.choerodon.mybatis.domain.AuditDomain;
import org.hzero.starter.keyencrypt.core.Encrypt;

import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.Table;
import javax.persistence.Transient;
import java.math.BigDecimal;
import java.util.Date;

/**
 * @author zhaotianxin
 * @date 2020-08-13 14:41
 */
@Table(name = "fd_status_field_value_setting")
@ModifyAudit
@VersionAudit
public class StatusFieldValueSettingDTO extends AuditDomain {

    @Id
    @GeneratedValue
    @Encrypt
    private Long id;

    @Encrypt
    private Long statusFieldSettingId;

    private Long projectId;

    @Encrypt
    private Long optionId;

    private String stringValue;

    private String fieldType;

    private String operateType;

    private BigDecimal numberValue;

    private BigDecimal numberAddValue;

    private String textValue;

    private Date dateValue;

    private BigDecimal dateAddValue;

    @Encrypt
    private Long userId;

    @Transient
    private String name;

    private Long organizationId;

    private Long customFieldId;

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

    public Long getStatusFieldSettingId() {
        return statusFieldSettingId;
    }

    public void setStatusFieldSettingId(Long statusFieldSettingId) {
        this.statusFieldSettingId = statusFieldSettingId;
    }

    public Long getProjectId() {
        return projectId;
    }

    public void setProjectId(Long projectId) {
        this.projectId = projectId;
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

    public BigDecimal getNumberAddValue() {
        return numberAddValue;
    }

    public void setNumberAddValue(BigDecimal numberAddValue) {
        this.numberAddValue = numberAddValue;
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

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public Long getOrganizationId() {
        return organizationId;
    }

    public void setOrganizationId(Long organizationId) {
        this.organizationId = organizationId;
    }
}
