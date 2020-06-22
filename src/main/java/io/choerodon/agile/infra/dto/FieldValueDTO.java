package io.choerodon.agile.infra.dto;

import io.choerodon.agile.infra.constants.EncryptionConstant;
import io.choerodon.mybatis.annotation.ModifyAudit;
import io.choerodon.mybatis.annotation.VersionAudit;
import io.choerodon.mybatis.domain.AuditDomain;
import org.hzero.starter.keyencrypt.core.Encrypt;

import javax.persistence.*;
import java.util.Date;

/**
 * @author shinan.chen
 * @since 2019/4/8
 */
@Table(name = "fd_field_value")
@ModifyAudit
@VersionAudit
public class FieldValueDTO extends AuditDomain {
    @Id
    @GeneratedValue
    @Encrypt/*(EncryptionConstant.FD_FIELD_VALUE)*/
    private Long id;
    @Encrypt/*(EncryptionConstant.AGILE_ISSUE)*/
    private Long instanceId;
    @Encrypt/*(EncryptionConstant.FD_OBJECT_SCHEME_FIELD)*/
    private Long fieldId;
    @Encrypt/*(EncryptionConstant.FD_FIELD_OPTION)*/
    private Long optionId;
    private String stringValue;
    private String numberValue;
    private String textValue;
    private Date dateValue;
    private Long projectId;
    private String schemeCode;

    @Transient
    private String optionValue;
    @Transient
    private String fieldType;

    public String getFieldType() {
        return fieldType;
    }

    public void setFieldType(String fieldType) {
        this.fieldType = fieldType;
    }

    public String getOptionValue() {
        return optionValue;
    }

    public void setOptionValue(String optionValue) {
        this.optionValue = optionValue;
    }

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public Long getInstanceId() {
        return instanceId;
    }

    public void setInstanceId(Long instanceId) {
        this.instanceId = instanceId;
    }

    public Long getFieldId() {
        return fieldId;
    }

    public void setFieldId(Long fieldId) {
        this.fieldId = fieldId;
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

    public String getNumberValue() {
        return numberValue;
    }

    public void setNumberValue(String numberValue) {
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

    public Long getProjectId() {
        return projectId;
    }

    public void setProjectId(Long projectId) {
        this.projectId = projectId;
    }

    public String getSchemeCode() {
        return schemeCode;
    }

    public void setSchemeCode(String schemeCode) {
        this.schemeCode = schemeCode;
    }
}
