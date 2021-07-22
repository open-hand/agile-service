package io.choerodon.agile.api.vo;

/**
 * @author zhaotianxin
 * @date 2021-07-14 22:54
 */
public class IssueLinkChangeVO {
    private Long issueId;

    private Long  issueTypeId;

    private Long statusId;

    private Long linkTypeId;

    private Long linkedIssueId;

    private Long linkIssueTypeId;

    private Long linkIssueStatusId;

    public Long getIssueId() {
        return issueId;
    }

    public void setIssueId(Long issueId) {
        this.issueId = issueId;
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

    public Long getLinkedIssueId() {
        return linkedIssueId;
    }

    public void setLinkedIssueId(Long linkedIssueId) {
        this.linkedIssueId = linkedIssueId;
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
}
