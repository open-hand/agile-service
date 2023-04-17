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
    @Transient
    private List<PageFieldDTO> pages;

    public List<PageFieldDTO> getPages() {
        return pages;
    }

    public ObjectSchemeFieldDTO setPages(List<PageFieldDTO> pages) {
        this.pages = pages;
        return this;
    }

    public String getRank() {
        return rank;
    }

    public ObjectSchemeFieldDTO setRank(String rank) {
        this.rank = rank;
        return this;
    }

    public String getCreatedLevel() {
        return createdLevel;
    }

    public ObjectSchemeFieldDTO setCreatedLevel(String createdLevel) {
        this.createdLevel = createdLevel;
        return this;
    }

    public List<ObjectSchemeFieldExtendDTO> getExtendFields() {
        return extendFields;
    }

    public ObjectSchemeFieldDTO setExtendFields(List<ObjectSchemeFieldExtendDTO> extendFields) {
        this.extendFields = extendFields;
        return this;
    }

    public String getFieldTypeName() {
        return fieldTypeName;
    }

    public ObjectSchemeFieldDTO setFieldTypeName(String fieldTypeName) {
        this.fieldTypeName = fieldTypeName;
        return this;
    }

    public Long getId() {
        return id;
    }

    public ObjectSchemeFieldDTO setId(Long id) {
        this.id = id;
        return this;
    }

    public String getCode() {
        return code;
    }

    public ObjectSchemeFieldDTO setCode(String code) {
        this.code = code;
        return this;
    }

    public String getName() {
        return name;
    }

    public ObjectSchemeFieldDTO setName(String name) {
        this.name = name;
        return this;
    }

    public String getDescription() {
        return description;
    }

    public ObjectSchemeFieldDTO setDescription(String description) {
        this.description = description;
        return this;
    }

    public String getFieldType() {
        return fieldType;
    }

    public ObjectSchemeFieldDTO setFieldType(String fieldType) {
        this.fieldType = fieldType;
        return this;
    }

    public String getDefaultValue() {
        return defaultValue;
    }

    public ObjectSchemeFieldDTO setDefaultValue(String defaultValue) {
        this.defaultValue = defaultValue;
        return this;
    }

    public Boolean getExtraConfig() {
        return extraConfig;
    }

    public ObjectSchemeFieldDTO setExtraConfig(Boolean extraConfig) {
        this.extraConfig = extraConfig;
        return this;
    }

    public Boolean getSystem() {
        return system;
    }

    public ObjectSchemeFieldDTO setSystem(Boolean system) {
        this.system = system;
        return this;
    }

    public Boolean getRequired() {
        return required;
    }

    public ObjectSchemeFieldDTO setRequired(Boolean required) {
        this.required = required;
        return this;
    }

    public String getContext() {
        return context;
    }

    public ObjectSchemeFieldDTO setContext(String context) {
        this.context = context;
        return this;
    }

    public String getSchemeCode() {
        return schemeCode;
    }

    public ObjectSchemeFieldDTO setSchemeCode(String schemeCode) {
        this.schemeCode = schemeCode;
        return this;
    }

    public Long getProjectId() {
        return projectId;
    }

    public ObjectSchemeFieldDTO setProjectId(Long projectId) {
        this.projectId = projectId;
        return this;
    }

    public Long getOrganizationId() {
        return organizationId;
    }

    public ObjectSchemeFieldDTO setOrganizationId(Long organizationId) {
        this.organizationId = organizationId;
        return this;
    }
}
