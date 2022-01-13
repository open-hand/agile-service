package io.choerodon.agile.infra.dto;

import io.choerodon.mybatis.annotation.ModifyAudit;
import io.choerodon.mybatis.annotation.VersionAudit;
import io.choerodon.mybatis.domain.AuditDomain;

import javax.persistence.*;
import java.math.BigDecimal;
import java.util.List;

/**
 * @author shinan.chen
 * @date 2018/8/8
 */
@Table(name = "fd_issue_type")
@ModifyAudit
@VersionAudit
public class IssueTypeDTO extends AuditDomain {
    @Id
    @GeneratedValue
    private Long id;

    private String icon;
    private String name;
    private String description;
    private Long organizationId;
    private String colour;
    private String typeCode;
    @Column(name = "is_initialize")
    private Boolean initialize;
    private Long projectId;
    @Transient
    private Boolean enabled;
    private Boolean referenced;
    private Long referenceId;
    private String source;
    @Transient
    private String applyType;

    public IssueTypeDTO(String icon, String name, String description, Long organizationId, String colour, String typeCode, Boolean initialize) {
        this.icon = icon;
        this.name = name;
        this.description = description;
        this.organizationId = organizationId;
        this.colour = colour;
        this.typeCode = typeCode;
        this.initialize = initialize;
    }

    public IssueTypeDTO() {
    }

    public IssueTypeDTO(Long organizationId, Long id) {
        this.organizationId = organizationId;
        this.id = id;
    }

    @Transient
    private BigDecimal sequence;

    @Transient
    private StatusMachineSchemeConfigDTO stateMachineSchemeConfig;

    @Transient
    private List<IssueTypeExtendDTO> issueTypeExtends;

    public List<IssueTypeExtendDTO> getIssueTypeExtends() {
        return issueTypeExtends;
    }

    public void setIssueTypeExtends(List<IssueTypeExtendDTO> issueTypeExtends) {
        this.issueTypeExtends = issueTypeExtends;
    }

    public String getSource() {
        return source;
    }

    public void setSource(String source) {
        this.source = source;
    }

    public Long getReferenceId() {
        return referenceId;
    }

    public void setReferenceId(Long referenceId) {
        this.referenceId = referenceId;
    }

    public Boolean getEnabled() {
        return enabled;
    }

    public void setEnabled(Boolean enabled) {
        this.enabled = enabled;
    }

    public Boolean getReferenced() {
        return referenced;
    }

    public void setReferenced(Boolean referenced) {
        this.referenced = referenced;
    }

    public Long getProjectId() {
        return projectId;
    }

    public void setProjectId(Long projectId) {
        this.projectId = projectId;
    }

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public Long getOrganizationId() {
        return organizationId;
    }

    public void setOrganizationId(Long organizationId) {
        this.organizationId = organizationId;
    }

    public BigDecimal getSequence() {
        return sequence;
    }

    public void setSequence(BigDecimal sequence) {
        this.sequence = sequence;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }


    public String getIcon() {
        return icon;
    }

    public void setIcon(String icon) {
        this.icon = icon;
    }

    public StatusMachineSchemeConfigDTO getStateMachineSchemeConfig() {
        return stateMachineSchemeConfig;
    }

    public void setStateMachineSchemeConfig(StatusMachineSchemeConfigDTO stateMachineSchemeConfig) {
        this.stateMachineSchemeConfig = stateMachineSchemeConfig;
    }

    public String getColour() {
        return colour;
    }

    public void setColour(String colour) {
        this.colour = colour;
    }

    public String getTypeCode() {
        return typeCode;
    }

    public void setTypeCode(String typeCode) {
        this.typeCode = typeCode;
    }

    public Boolean getInitialize() {
        return initialize;
    }

    public void setInitialize(Boolean initialize) {
        this.initialize = initialize;
    }

    public String getApplyType() {
        return applyType;
    }

    public void setApplyType(String applyType) {
        this.applyType = applyType;
    }

    @Override
    public String toString() {
        return "IssueTypeDTO{" +
                "id=" + id +
                ", icon='" + icon + '\'' +
                ", name='" + name + '\'' +
                ", description='" + description + '\'' +
                ", organizationId=" + organizationId +
                ", colour='" + colour + '\'' +
                ", typeCode='" + typeCode + '\'' +
                ", initialize=" + initialize +
                ", projectId=" + projectId +
                ", enabled=" + enabled +
                ", referenced=" + referenced +
                ", sequence=" + sequence +
                ", stateMachineSchemeConfig=" + stateMachineSchemeConfig +
                '}';
    }
}
