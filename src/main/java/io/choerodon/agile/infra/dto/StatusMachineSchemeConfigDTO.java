package io.choerodon.agile.infra.dto;

import io.choerodon.mybatis.annotation.ModifyAudit;
import io.choerodon.mybatis.annotation.VersionAudit;
import io.choerodon.mybatis.domain.AuditDomain;

import javax.persistence.GeneratedValue;

import javax.persistence.Id;
import javax.persistence.Table;

/**
 * @author peng.jiang@hand-china.com
 */
@Table(name = "fd_status_machine_scheme_config")
@ModifyAudit
@VersionAudit
public class StatusMachineSchemeConfigDTO extends AuditDomain {

    @Id
    @GeneratedValue
    private Long id;
    private Long schemeId;
    private Long issueTypeId;
    private Long stateMachineId;
    private Boolean isDefault;
    private Integer sequence;
    private Long organizationId;

    public StatusMachineSchemeConfigDTO() {
    }

    public StatusMachineSchemeConfigDTO(Long schemeId, Long issueTypeId, Long organizationId) {
        this.schemeId = schemeId;
        this.issueTypeId = issueTypeId;
        this.organizationId = organizationId;
    }

    public StatusMachineSchemeConfigDTO(Long stateMachineId, Boolean isDefault, Long organizationId) {
        this.stateMachineId = stateMachineId;
        this.isDefault = isDefault;
        this.organizationId = organizationId;
    }

    public StatusMachineSchemeConfigDTO(Long schemeId, Long issueTypeId, Long stateMachineId, Boolean isDefault, Long organizationId) {
        this.schemeId = schemeId;
        this.issueTypeId = issueTypeId;
        this.stateMachineId = stateMachineId;
        this.isDefault = isDefault;
        this.organizationId = organizationId;
    }

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public Long getSchemeId() {
        return schemeId;
    }

    public void setSchemeId(Long schemeId) {
        this.schemeId = schemeId;
    }

    public Long getIssueTypeId() {
        return issueTypeId;
    }

    public void setIssueTypeId(Long issueTypeId) {
        this.issueTypeId = issueTypeId;
    }

    public Long getStateMachineId() {
        return stateMachineId;
    }

    public void setStateMachineId(Long stateMachineId) {
        this.stateMachineId = stateMachineId;
    }

    public Boolean getDefault() {
        return isDefault;
    }

    public void setDefault(Boolean aDefault) {
        isDefault = aDefault;
    }

    public Integer getSequence() {
        return sequence;
    }

    public void setSequence(Integer sequence) {
        this.sequence = sequence;
    }

    public Long getOrganizationId() {
        return organizationId;
    }

    public void setOrganizationId(Long organizationId) {
        this.organizationId = organizationId;
    }
}
