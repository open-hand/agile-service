package io.choerodon.agile.infra.dto;

import io.choerodon.mybatis.annotation.ModifyAudit;
import io.choerodon.mybatis.annotation.VersionAudit;
import io.choerodon.mybatis.domain.AuditDomain;

import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.Table;
import java.util.Objects;

/**
 * @author superlee
 * @since 2021-11-10
 */
@Table(name = "agile_issue_predecessor")
@ModifyAudit
@VersionAudit
public class IssuePredecessorDTO extends AuditDomain {

    @Id
    @GeneratedValue
    private Long id;

    private Long issueId;

    private Long predecessorId;

    private String predecessorType;

    private Long projectId;

    private Long organizationId;

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public Long getIssueId() {
        return issueId;
    }

    public void setIssueId(Long issueId) {
        this.issueId = issueId;
    }

    public Long getPredecessorId() {
        return predecessorId;
    }

    public void setPredecessorId(Long predecessorId) {
        this.predecessorId = predecessorId;
    }

    public String getPredecessorType() {
        return predecessorType;
    }

    public void setPredecessorType(String predecessorType) {
        this.predecessorType = predecessorType;
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

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof IssuePredecessorDTO)) return false;
        IssuePredecessorDTO that = (IssuePredecessorDTO) o;
        return Objects.equals(getIssueId(), that.getIssueId()) &&
                Objects.equals(getPredecessorId(), that.getPredecessorId()) &&
                Objects.equals(getPredecessorType(), that.getPredecessorType()) &&
                Objects.equals(getProjectId(), that.getProjectId()) &&
                Objects.equals(getOrganizationId(), that.getOrganizationId());
    }

    @Override
    public int hashCode() {
        return Objects.hash(getIssueId(), getPredecessorId(), getPredecessorType(), getProjectId(), getOrganizationId());
    }
}
