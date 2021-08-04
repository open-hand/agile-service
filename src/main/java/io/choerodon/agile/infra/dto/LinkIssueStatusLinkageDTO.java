package io.choerodon.agile.infra.dto;

import io.choerodon.mybatis.annotation.ModifyAudit;
import io.choerodon.mybatis.annotation.VersionAudit;
import io.choerodon.mybatis.domain.AuditDomain;

import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.Table;

/**
 * @author zhaotianxin
 * @date 2021-06-09 10:59
 */
@Table(name = "fd_link_issue_status_linkage")
@ModifyAudit
@VersionAudit
public class LinkIssueStatusLinkageDTO extends AuditDomain {
    @Id
    @GeneratedValue
    private Long id;

    private Long issueTypeId;

    private Long statusId;

    private Long linkTypeId;

    private Long linkIssueTypeId;

    private Long linkIssueStatusId;

    private Long projectId;

    private Long organizationId;

    private Boolean isTriggered;

    public LinkIssueStatusLinkageDTO() {
    }

    public LinkIssueStatusLinkageDTO(Long projectId, Long organizationId) {
        this.projectId = projectId;
        this.organizationId = organizationId;
    }

    public LinkIssueStatusLinkageDTO(Long issueTypeId, Long statusId, Long projectId, Long organizationId) {
        this.issueTypeId = issueTypeId;
        this.statusId = statusId;
        this.projectId = projectId;
        this.organizationId = organizationId;
    }

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public Long getIssueTypeId() {
        return issueTypeId;
    }

    public void setIssueTypeId(Long issueTypeId) {
        this.issueTypeId = issueTypeId;
    }

    public Long getStatusId() {
        return statusId;
    }

    public void setStatusId(Long statusId) {
        this.statusId = statusId;
    }

    public Long getLinkTypeId() {
        return linkTypeId;
    }

    public void setLinkTypeId(Long linkTypeId) {
        this.linkTypeId = linkTypeId;
    }

    public Long getLinkIssueTypeId() {
        return linkIssueTypeId;
    }

    public void setLinkIssueTypeId(Long linkIssueTypeId) {
        this.linkIssueTypeId = linkIssueTypeId;
    }

    public Long getLinkIssueStatusId() {
        return linkIssueStatusId;
    }

    public void setLinkIssueStatusId(Long linkIssueStatusId) {
        this.linkIssueStatusId = linkIssueStatusId;
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

    public Boolean getTriggered() {
        return isTriggered;
    }

    public void setTriggered(Boolean triggered) {
        isTriggered = triggered;
    }
}
