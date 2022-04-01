package io.choerodon.agile.api.vo;

import org.hzero.starter.keyencrypt.core.Encrypt;

/**
 * @author huaxin.deng@hand-china.com
 * @since 2022/3/31
 */
public class IssueStatusLinkageVO {
    @Encrypt
    private Long id;

    @Encrypt
    private Long issueTypeId;

    @Encrypt
    private Long statusId;

    @Encrypt
    private Long linkTypeId;

    private IssueLinkTypeVO linkTypeVO;

    private String predecessorType;

    @Encrypt
    private Long linkageIssueTypeId;

    private IssueTypeVO linkageIssueType;

    @Encrypt
    private Long linkageIssueStatusId;

    private StatusVO linkageIssueStatus;

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

    public IssueLinkTypeVO getLinkTypeVO() {
        return linkTypeVO;
    }

    public void setLinkTypeVO(IssueLinkTypeVO linkTypeVO) {
        this.linkTypeVO = linkTypeVO;
    }

    public String getPredecessorType() {
        return predecessorType;
    }

    public void setPredecessorType(String predecessorType) {
        this.predecessorType = predecessorType;
    }

    public Long getLinkageIssueTypeId() {
        return linkageIssueTypeId;
    }

    public void setLinkageIssueTypeId(Long linkageIssueTypeId) {
        this.linkageIssueTypeId = linkageIssueTypeId;
    }

    public IssueTypeVO getLinkageIssueType() {
        return linkageIssueType;
    }

    public void setLinkageIssueType(IssueTypeVO linkageIssueType) {
        this.linkageIssueType = linkageIssueType;
    }

    public Long getLinkageIssueStatusId() {
        return linkageIssueStatusId;
    }

    public void setLinkageIssueStatusId(Long linkageIssueStatusId) {
        this.linkageIssueStatusId = linkageIssueStatusId;
    }

    public StatusVO getLinkageIssueStatus() {
        return linkageIssueStatus;
    }

    public void setLinkageIssueStatus(StatusVO linkageIssueStatus) {
        this.linkageIssueStatus = linkageIssueStatus;
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
