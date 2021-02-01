package io.choerodon.agile.infra.dto;

import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.Table;

import io.choerodon.mybatis.annotation.ModifyAudit;
import io.choerodon.mybatis.annotation.VersionAudit;
import io.choerodon.mybatis.domain.AuditDomain;

/**
 * @author chihao.ran@hand-china.com
 * 2021/01/13 16:07
 */
@Table(name = "agile_issue_static_file_rel")
@ModifyAudit
@VersionAudit
public class StaticFileIssueRelDTO extends AuditDomain {
    @Id
    @GeneratedValue
    private Long id;
    private Long staticFileId;
    private Long issueId;
    private Long projectId;
    private Long organizationId;

    public StaticFileIssueRelDTO() {
    }

    public StaticFileIssueRelDTO(Long staticFileId, Long issueId, Long projectId, Long organizationId) {
        this.staticFileId = staticFileId;
        this.issueId = issueId;
        this.projectId = projectId;
        this.organizationId = organizationId;
    }

    @Override
    public String toString() {
        return "StaticFileIssueDTO{" +
                "id=" + id +
                ", staticFileId=" + staticFileId +
                ", issueId=" + issueId +
                ", projectId=" + projectId +
                ", organizationId=" + organizationId +
                "} " + super.toString();
    }

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public Long getStaticFileId() {
        return staticFileId;
    }

    public void setStaticFileId(Long staticFileId) {
        this.staticFileId = staticFileId;
    }

    public Long getIssueId() {
        return issueId;
    }

    public void setIssueId(Long issueId) {
        this.issueId = issueId;
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
