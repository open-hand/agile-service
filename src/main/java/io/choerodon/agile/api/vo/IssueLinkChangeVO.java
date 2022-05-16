package io.choerodon.agile.api.vo;

import io.swagger.annotations.ApiModelProperty;

/**
 * @author zhaotianxin
 * @date 2021-07-14 22:54
 */
public class IssueLinkChangeVO {
    @ApiModelProperty(value = "问题id")
    private Long issueId;
    @ApiModelProperty(value = "问题类型id")
    private Long  issueTypeId;
    @ApiModelProperty(value = "状态id")
    private Long statusId;
    @ApiModelProperty(value = "关联类型id")
    private Long linkTypeId;
    @ApiModelProperty(value = "关联问题id")
    private Long linkedIssueId;
    @ApiModelProperty(value = "关联问题类型id")
    private Long linkIssueTypeId;
    @ApiModelProperty(value = "关联问题状态id")
    private Long linkIssueStatusId;
    @ApiModelProperty(value = "是否可以被触发")
    private Boolean isTriggered;
    @ApiModelProperty(value = "状态联动设置id")
    private Long linkSettingId;

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

    public Boolean getTriggered() {
        return isTriggered;
    }

    public void setTriggered(Boolean triggered) {
        isTriggered = triggered;
    }

    public Long getLinkSettingId() {
        return linkSettingId;
    }

    public void setLinkSettingId(Long linkSettingId) {
        this.linkSettingId = linkSettingId;
    }
}
