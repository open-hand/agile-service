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
@Table(name = "fd_field_cascade_rule_option")
@ModifyAudit
@VersionAudit
public class FieldCascadeRuleOptionDTO extends AuditDomain {
    @Id
    @GeneratedValue
    private Long id;
    private Long fieldCascadeRuleId;
    private Long cascadeOptionId;
    @Column(name = "is_default")
    private Boolean defaultOption;
    private Long projectId;
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
}
