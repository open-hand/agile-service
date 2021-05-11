package io.choerodon.agile.infra.dto;

import io.choerodon.mybatis.annotation.ModifyAudit;
import io.choerodon.mybatis.annotation.VersionAudit;
import io.choerodon.mybatis.domain.AuditDomain;

import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.Table;

/**
 * @author superlee
 * @since 2021-03-25
 */
@Table(name = "agile_tag_issue_rel")
@ModifyAudit
@VersionAudit
public class TagIssueRelDTO extends AuditDomain {

    @Id
    @GeneratedValue
    private Long id;

    private String appServiceCode;

    private String tagName;

    private Long issueId;

    private Long tagProjectId;

    private Long projectId;

    private Long organizationId;

    public Long getTagProjectId() {
        return tagProjectId;
    }

    public void setTagProjectId(Long tagProjectId) {
        this.tagProjectId = tagProjectId;
    }

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public String getAppServiceCode() {
        return appServiceCode;
    }

    public void setAppServiceCode(String appServiceCode) {
        this.appServiceCode = appServiceCode;
    }

    public String getTagName() {
        return tagName;
    }

    public void setTagName(String tagName) {
        this.tagName = tagName;
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
