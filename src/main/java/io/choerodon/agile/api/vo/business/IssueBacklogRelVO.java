package io.choerodon.agile.api.vo.business;

import io.swagger.annotations.ApiModelProperty;

/**
 * @author huaxin.deng@hand-china.com 2021-12-21 19:06:56
 */
public class IssueBacklogRelVO {
    @ApiModelProperty(value = "问题id")
    private Long issueId;
    @ApiModelProperty(value = "需求id")
    private Long backlogId;
    @ApiModelProperty(value = "概要")
    private String summary;
    @ApiModelProperty(value = "需求编码")
    private String backlogNum;
    @ApiModelProperty(value = "来源")
    private String source;

    public Long getIssueId() {
        return issueId;
    }

    public void setIssueId(Long issueId) {
        this.issueId = issueId;
    }

    public Long getBacklogId() {
        return backlogId;
    }

    public void setBacklogId(Long backlogId) {
        this.backlogId = backlogId;
    }

    public String getSummary() {
        return summary;
    }

    public void setSummary(String summary) {
        this.summary = summary;
    }

    public String getBacklogNum() {
        return backlogNum;
    }

    public void setBacklogNum(String backlogNum) {
        this.backlogNum = backlogNum;
    }

    public String getSource() {
        return source;
    }

    public void setSource(String source) {
        this.source = source;
    }
}
