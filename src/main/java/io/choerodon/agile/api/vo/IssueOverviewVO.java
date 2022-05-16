package io.choerodon.agile.api.vo;

import java.math.BigDecimal;

import io.choerodon.mybatis.domain.AuditDomain;
import io.swagger.annotations.ApiModelProperty;

/**
 * @author jiaxu.cui@hand-china.com 2020/7/1 下午3:07
 */
public class IssueOverviewVO extends AuditDomain {
    @ApiModelProperty(value = "问题id")
    private Long issueId;
    @ApiModelProperty(value = "是否已完成")
    private Boolean completed;
    @ApiModelProperty(value = "报告人id")
    private Long reporterId;
    @ApiModelProperty(value = "经办人id")
    private Long assigneeId;
    @ApiModelProperty(value = "类别编码")
    private String categoryCode;
    @ApiModelProperty(value = "问题类型编码")
    private String typeCode;
    @ApiModelProperty(value = "故事点")
    private BigDecimal storyPoints;
    @ApiModelProperty(value = "预计时间")
    private BigDecimal remainingTime;
    @ApiModelProperty(value = "状态id")
    private Long statusId;
    @ApiModelProperty(value = "问题类型id")
    private Long issueTypeId;

    public BigDecimal getStoryPoints() {
        return storyPoints;
    }

    public void setStoryPoints(BigDecimal storyPoints) {
        this.storyPoints = storyPoints;
    }

    public BigDecimal getRemainingTime() {
        return remainingTime;
    }

    public void setRemainingTime(BigDecimal remainingTime) {
        this.remainingTime = remainingTime;
    }

    public String getTypeCode() {
        return typeCode;
    }

    public void setTypeCode(String typeCode) {
        this.typeCode = typeCode;
    }

    public Long getIssueId() {
        return issueId;
    }

    public void setIssueId(Long issueId) {
        this.issueId = issueId;
    }

    public Boolean getCompleted() {
        return completed;
    }

    public void setCompleted(Boolean completed) {
        this.completed = completed;
    }

    public Long getReporterId() {
        return reporterId;
    }

    public void setReporterId(Long reporterId) {
        this.reporterId = reporterId;
    }

    public Long getAssigneeId() {
        return assigneeId;
    }

    public void setAssigneeId(Long assigneeId) {
        this.assigneeId = assigneeId;
    }

    public String getCategoryCode() {
        return categoryCode;
    }

    public void setCategoryCode(String categoryCode) {
        this.categoryCode = categoryCode;
    }

    public Long getStatusId() {
        return statusId;
    }

    public void setStatusId(Long statusId) {
        this.statusId = statusId;
    }

    public Long getIssueTypeId() {
        return issueTypeId;
    }

    public void setIssueTypeId(Long issueTypeId) {
        this.issueTypeId = issueTypeId;
    }
}
