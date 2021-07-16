package io.choerodon.agile.infra.dto;

import javax.persistence.Column;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.Table;

import io.choerodon.mybatis.annotation.ModifyAudit;
import io.choerodon.mybatis.annotation.VersionAudit;
import io.choerodon.mybatis.domain.AuditDomain;

/**
 * @author chihao.ran@hand-china.com
 * 2021/07/13 15:59
 */
@Table(name = "fd_field_cascade_rule")
@ModifyAudit
@VersionAudit
public class FieldCascadeRuleDTO extends AuditDomain {
    @Id
    @GeneratedValue
    private Long id;
    private Long issueTypeId;
    private Long fieldId;
    private Long fieldOptionId;
    private Long cascadeFieldId;
    @Column(name = "is_hidden")
    private Boolean hidden;
    @Column(name = "is_required")
    private Boolean required;
    private String defaultValue;
    private Long organizationId;
    private Long projectId;

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public Long getIssueTypeId() {
        return issueTypeId;
    }

    public void setIssueTypeId(Long issueTypeId) {
        this.issueTypeId = issueTypeId;
    }

    public Long getFieldId() {
        return fieldId;
    }

    public void setFieldId(Long fieldId) {
        this.fieldId = fieldId;
    }

    public Long getFieldOptionId() {
        return fieldOptionId;
    }

    public void setFieldOptionId(Long fieldOptionId) {
        this.fieldOptionId = fieldOptionId;
    }

    public Long getCascadeFieldId() {
        return cascadeFieldId;
    }

    public void setCascadeFieldId(Long cascadeFieldId) {
        this.cascadeFieldId = cascadeFieldId;
    }

    public Boolean getHidden() {
        return hidden;
    }

    public void setHidden(Boolean hidden) {
        this.hidden = hidden;
    }

    public Boolean getRequired() {
        return required;
    }

    public void setRequired(Boolean required) {
        this.required = required;
    }

    public String getDefaultValue() {
        return defaultValue;
    }

    public void setDefaultValue(String defaultValue) {
        this.defaultValue = defaultValue;
    }

    public Long getOrganizationId() {
        return organizationId;
    }

    public void setOrganizationId(Long organizationId) {
        this.organizationId = organizationId;
    }

    public Long getProjectId() {
        return projectId;
    }

    public void setProjectId(Long projectId) {
        this.projectId = projectId;
    }
}
