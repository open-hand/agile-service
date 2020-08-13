package io.choerodon.agile.infra.dto;


import io.choerodon.mybatis.annotation.ModifyAudit;
import io.choerodon.mybatis.annotation.VersionAudit;
import io.choerodon.mybatis.domain.AuditDomain;
import org.hzero.starter.keyencrypt.core.Encrypt;

import javax.persistence.*;
import java.util.List;

/**
 * @author shinan.chen
 * @since 2019/3/29
 */
@Table(name = "fd_object_scheme_field")
@ModifyAudit
@VersionAudit
public class ObjectSchemeFieldDTO extends AuditDomain {
    @Id
    @GeneratedValue
    @Encrypt
    private Long id;
    private String code;
    private String name;
    private String description;
    private String fieldType;
    private String defaultValue;
    private Boolean extraConfig;
    @Column(name = "is_system")
    private Boolean system;
    @Column(name = "is_required")
    private Boolean required;
    private String context;
    private String schemeCode;
    private Long projectId;
    private Long organizationId;
    @Transient
    private String fieldTypeName;
    @Transient
    private List<ObjectSchemeFieldExtendDTO> extendFields;
    @Transient
    private String createdLevel;
    @Transient
    private String rank;

    public String getRank() {
        return rank;
    }

    public void setRank(String rank) {
        this.rank = rank;
    }

    public String getCreatedLevel() {
        return createdLevel;
    }

    public void setCreatedLevel(String createdLevel) {
        this.createdLevel = createdLevel;
    }

    public List<ObjectSchemeFieldExtendDTO> getExtendFields() {
        return extendFields;
    }

    public void setExtendFields(List<ObjectSchemeFieldExtendDTO> extendFields) {
        this.extendFields = extendFields;
    }

    public String getFieldTypeName() {
        return fieldTypeName;
    }

    public void setFieldTypeName(String fieldTypeName) {
        this.fieldTypeName = fieldTypeName;
    }

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public String getCode() {
        return code;
    }

    public void setCode(String code) {
        this.code = code;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public String getFieldType() {
        return fieldType;
    }

    public void setFieldType(String fieldType) {
        this.fieldType = fieldType;
    }

    public String getDefaultValue() {
        return defaultValue;
    }

    public void setDefaultValue(String defaultValue) {
        this.defaultValue = defaultValue;
    }

    public Boolean getExtraConfig() {
        return extraConfig;
    }

    public void setExtraConfig(Boolean extraConfig) {
        this.extraConfig = extraConfig;
    }

    public Boolean getSystem() {
        return system;
    }

    public void setSystem(Boolean system) {
        this.system = system;
    }

    public Boolean getRequired() {
        return required;
    }

    public void setRequired(Boolean required) {
        this.required = required;
    }

    public String getContext() {
        return context;
    }

    public void setContext(String context) {
        this.context = context;
    }

    public String getSchemeCode() {
        return schemeCode;
    }

    public void setSchemeCode(String schemeCode) {
        this.schemeCode = schemeCode;
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
}
