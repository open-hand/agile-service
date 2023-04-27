package io.choerodon.agile.api.vo.business;

import io.swagger.annotations.ApiModelProperty;

/**
 * @author huaxin.deng@hand-china.com 2021-12-21 19:06:56
 */
public class BacklogIssueRelVO {
    @ApiModelProperty(value = "关系id")
    private Long backlogIssueRelId;
    @ApiModelProperty(value = "关联类型")
    private String linkType;
    @ApiModelProperty(value = "工作项id")
    private Long issueId;
    @ApiModelProperty(value = "需求id")
    private Long backlogId;
    @ApiModelProperty(value = "项目id")
    private Long projectId;
    @ApiModelProperty(value = "概要")
    private String summary;
    @ApiModelProperty(value = "需求编码")
    private String backlogNum;
    @ApiModelProperty(value = "工作项是否已完成")
    private Boolean issueCompletedFlag;

    @ApiModelProperty(value = "工作项状态类型")
    private String issueStatusType;

    @ApiModelProperty(value = "需求状态编码")
    private String backlogStatusCode;

    @ApiModelProperty(value = "需求状态id")
    private Long backlogStatusId;

    @ApiModelProperty(value = "工作项状态id")
    private Long issueStatusId;
    @ApiModelProperty(value = "来源")
    private String source;

    /**
     * @return 关系id
     */
    public Long getBacklogIssueRelId() {
        return backlogIssueRelId;
    }

    public BacklogIssueRelVO setBacklogIssueRelId(Long backlogIssueRelId) {
        this.backlogIssueRelId = backlogIssueRelId;
        return this;
    }

    /**
     * @return 关联类型
     */
    public String getLinkType() {
        return linkType;
    }

    public BacklogIssueRelVO setLinkType(String linkType) {
        this.linkType = linkType;
        return this;
    }

    /**
     * @return 工作项id
     */
    public Long getIssueId() {
        return issueId;
    }

    public BacklogIssueRelVO setIssueId(Long issueId) {
        this.issueId = issueId;
        return this;
    }

    /**
     * @return 需求id
     */
    public Long getBacklogId() {
        return backlogId;
    }

    public BacklogIssueRelVO setBacklogId(Long backlogId) {
        this.backlogId = backlogId;
        return this;
    }

    /**
     * @return 项目id
     */
    public Long getProjectId() {
        return projectId;
    }

    public BacklogIssueRelVO setProjectId(Long projectId) {
        this.projectId = projectId;
        return this;
    }

    /**
     * @return 概要
     */
    public String getSummary() {
        return summary;
    }

    public BacklogIssueRelVO setSummary(String summary) {
        this.summary = summary;
        return this;
    }

    /**
     * @return 需求编码
     */
    public String getBacklogNum() {
        return backlogNum;
    }

    public BacklogIssueRelVO setBacklogNum(String backlogNum) {
        this.backlogNum = backlogNum;
        return this;
    }

    /**
     * @return 工作项是否已完成
     */
    public Boolean getIssueCompletedFlag() {
        return issueCompletedFlag;
    }

    public BacklogIssueRelVO setIssueCompletedFlag(Boolean issueCompletedFlag) {
        this.issueCompletedFlag = issueCompletedFlag;
        return this;
    }

    /**
     * @return 工作项状态类型
     */
    public String getIssueStatusType() {
        return issueStatusType;
    }

    public BacklogIssueRelVO setIssueStatusType(String issueStatusType) {
        this.issueStatusType = issueStatusType;
        return this;
    }

    /**
     * @return 需求状态编码
     */
    public String getBacklogStatusCode() {
        return backlogStatusCode;
    }

    public BacklogIssueRelVO setBacklogStatusCode(String backlogStatusCode) {
        this.backlogStatusCode = backlogStatusCode;
        return this;
    }

    /**
     * @return 需求状态id
     */
    public Long getBacklogStatusId() {
        return backlogStatusId;
    }

    public BacklogIssueRelVO setBacklogStatusId(Long backlogStatusId) {
        this.backlogStatusId = backlogStatusId;
        return this;
    }

    /**
     * @return 工作项状态id
     */
    public Long getIssueStatusId() {
        return issueStatusId;
    }

    public BacklogIssueRelVO setIssueStatusId(Long issueStatusId) {
        this.issueStatusId = issueStatusId;
        return this;
    }

    /**
     * @return 来源
     */
    public String getSource() {
        return source;
    }

    public BacklogIssueRelVO setSource(String source) {
        this.source = source;
        return this;
    }
}
