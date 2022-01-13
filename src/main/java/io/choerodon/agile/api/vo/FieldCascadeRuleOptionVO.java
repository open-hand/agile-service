package io.choerodon.agile.api.vo;

import org.hzero.starter.keyencrypt.core.Encrypt;

import javax.validation.constraints.NotNull;

import io.choerodon.mybatis.domain.AuditDomain;

/**
 * @author chihao.ran@hand-china.com
 * 2021/07/13 15:59
 */

public class FieldCascadeRuleOptionVO extends AuditDomain {

    @Encrypt
    private Long id;
    @Encrypt
    private Long fieldCascadeRuleId;
    @Encrypt
    private Long cascadeOptionId;
    private String cascadeOptionName;
    private Long projectId;
    private Boolean defaultOption;
    private Long organizationId;

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public Long getFieldCascadeRuleId() {
        return fieldCascadeRuleId;
    }

    public void setFieldCascadeRuleId(Long fieldCascadeRuleId) {
        this.fieldCascadeRuleId = fieldCascadeRuleId;
    }

    public Long getCascadeOptionId() {
        return cascadeOptionId;
    }

    public void setCascadeOptionId(Long cascadeOptionId) {
        this.cascadeOptionId = cascadeOptionId;
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

    public Boolean getDefaultOption() {
        return defaultOption;
    }

    public void setDefaultOption(Boolean defaultOption) {
        this.defaultOption = defaultOption;
    }

    public String getCascadeOptionName() {
        return cascadeOptionName;
    }

    public void setCascadeOptionName(String cascadeOptionName) {
        this.cascadeOptionName = cascadeOptionName;
    }
}
