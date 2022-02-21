package io.choerodon.agile.infra.dto;


import io.choerodon.mybatis.annotation.ModifyAudit;
import io.choerodon.mybatis.annotation.VersionAudit;
import io.choerodon.mybatis.domain.AuditDomain;
import org.hzero.starter.keyencrypt.core.Encrypt;

import javax.persistence.*;

/**
 * @author superlee
 * @since 2020-08-10
 */
@Table(name = "fd_object_scheme_field_extend")
@ModifyAudit
@VersionAudit
public class ObjectSchemeFieldExtendDTO extends AuditDomain {
    @Id
    @GeneratedValue
    @Encrypt
    private Long id;

    private Long organizationId;

    private Long projectId;

    private Long issueTypeId;

    @Transient
    private String issueTypeName;

    private Long fieldId;

    private String issueType;

    @Column(name = "rank")
    private String rank;

    @Column(name = "is_required")
    private Boolean required;

    @Column(name = "is_created")
    private Boolean created;

    @Column(name = "is_edited")
    private Boolean edited;

    private String defaultValue;

    private Boolean extraConfig;

    @Transient
    private String fieldCode;

    public ObjectSchemeFieldExtendDTO() {
    }

    public ObjectSchemeFieldExtendDTO(Boolean required,
                                      Boolean created,
                                      Boolean edited) {
        this.required = required;
        this.created = created;
        this.edited = edited;
    }

    public ObjectSchemeFieldExtendDTO(Long projectId, Long issueTypeId, Long fieldId) {
        this.projectId = projectId;
        this.issueTypeId = issueTypeId;
        this.fieldId = fieldId;
    }

    public String getDefaultValue() {
        return defaultValue;
    }

    public void setDefaultValue(String defaultValue) {
        this.defaultValue = defaultValue;
    }

    public String getIssueTypeName() {
        return issueTypeName;
    }

    public void setIssueTypeName(String issueTypeName) {
        this.issueTypeName = issueTypeName;
    }

    public String getRank() {
        return rank;
    }

    public void setRank(String rank) {
        this.rank = rank;
    }

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
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

    public String getIssueType() {
        return issueType;
    }

    public void setIssueType(String issueType) {
        this.issueType = issueType;
    }

    public Boolean getRequired() {
        return required;
    }

    public void setRequired(Boolean required) {
        this.required = required;
    }

    public Boolean getCreated() {
        return created;
    }

    public void setCreated(Boolean created) {
        this.created = created;
    }

    public Boolean getEdited() {
        return edited;
    }

    public void setEdited(Boolean edited) {
        this.edited = edited;
    }

    public Boolean getExtraConfig() {
        return extraConfig;
    }

    public void setExtraConfig(Boolean extraConfig) {
        this.extraConfig = extraConfig;
    }

    public String getFieldCode() {
        return fieldCode;
    }

    public void setFieldCode(String fieldCode) {
        this.fieldCode = fieldCode;
    }
}
