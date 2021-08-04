package io.choerodon.agile.api.vo;

import org.hzero.starter.keyencrypt.core.Encrypt;

/**
 * @author zhaotianxin
 * @date 2021-06-09 11:04
 */
public class LinkIssueStatusLinkageVO {
    @Encrypt
    private Long id;

    @Encrypt
    private Long issueTypeId;

    @Encrypt
    private Long statusId;

    @Encrypt
    private Long linkTypeId;

    @Encrypt
    private Long linkIssueTypeId;

    private IssueTypeVO linkIssueType;

    @Encrypt
    private Long linkIssueStatusId;

    private StatusVO linkIssueStatus;

    private IssueLinkTypeVO linkTypeVO;

    private StatusVO statusVO;

    private IssueTypeVO issueTypeVO;

    private Long projectId;

    private Long organizationId;

    private Boolean isTriggered;

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

    public IssueTypeVO getLinkIssueType() {
        return linkIssueType;
    }

    public void setLinkIssueType(IssueTypeVO linkIssueType) {
        this.linkIssueType = linkIssueType;
    }

    public Long getLinkIssueStatusId() {
        return linkIssueStatusId;
    }

    public void setLinkIssueStatusId(Long linkIssueStatusId) {
        this.linkIssueStatusId = linkIssueStatusId;
    }

    public StatusVO getLinkIssueStatus() {
        return linkIssueStatus;
    }

    public void setLinkIssueStatus(StatusVO linkIssueStatus) {
        this.linkIssueStatus = linkIssueStatus;
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

    public IssueLinkTypeVO getLinkTypeVO() {
        return linkTypeVO;
    }

    public void setLinkTypeVO(IssueLinkTypeVO linkTypeVO) {
        this.linkTypeVO = linkTypeVO;
    }

    public Boolean getTriggered() {
        return isTriggered;
    }

    public void setTriggered(Boolean triggered) {
        isTriggered = triggered;
    }

    public StatusVO getStatusVO() {
        return statusVO;
    }

    public void setStatusVO(StatusVO statusVO) {
        this.statusVO = statusVO;
    }

    public IssueTypeVO getIssueTypeVO() {
        return issueTypeVO;
    }

    public void setIssueTypeVO(IssueTypeVO issueTypeVO) {
        this.issueTypeVO = issueTypeVO;
    }
}
