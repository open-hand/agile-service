package io.choerodon.agile.api.vo;

import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

/**
 * @author huaxin.deng@hand-china.com
 * @since 2022/3/31
 */
public class IssueStatusLinkageVO {
    @Encrypt
    @ApiModelProperty(value = "id")
    private Long id;

    @Encrypt
    @ApiModelProperty(value = "问题类型id")
    private Long issueTypeId;

    @Encrypt
    @ApiModelProperty(value = "状态id")
    private Long statusId;

    @Encrypt
    @ApiModelProperty(value = "关联类型id")
    private Long linkTypeId;
    @ApiModelProperty(value = "关联类型")
    private IssueLinkTypeVO linkTypeVO;
    @ApiModelProperty(value = "前置项类型")
    private String predecessorType;

    @Encrypt
    @ApiModelProperty(value = "关联的问题id")
    private Long linkageIssueTypeId;
    @ApiModelProperty(value = "关联的问题类型")
    private IssueTypeVO linkageIssueType;

    @Encrypt
    @ApiModelProperty(value = "关联的问题状态id")
    private Long linkageIssueStatusId;
    @ApiModelProperty(value = "关联的问题状态")
    private StatusVO linkageIssueStatus;
    @ApiModelProperty(value = "状态")
    private StatusVO statusVO;
    @ApiModelProperty(value = "问题类型")
    private IssueTypeVO issueTypeVO;
    @ApiModelProperty(value = "项目id")
    private Long projectId;
    @ApiModelProperty(value = "组织id")
    private Long organizationId;
    @ApiModelProperty(value = "是否被触发器修改")
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
