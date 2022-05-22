package io.choerodon.agile.api.vo;

import io.swagger.annotations.ApiModelProperty;

import java.util.Date;
import java.util.List;

/**
 * @author huaxin.deng@hand-china.com 2021-08-02 20:55:16
 */
public class IssueDailyWorkVO {
    @ApiModelProperty(value = "问题id")
    private Long issueId;
    @ApiModelProperty(value = "概要")
    private String summary;
    @ApiModelProperty(value = "项目id")
    private Long projectId;
    @ApiModelProperty(value = "问题类型编码")
    private String typeCode;
    @ApiModelProperty(value = "问题编码")
    private String issueNum;
    @ApiModelProperty(value = "报告人id")
    private Long reporterId;
    @ApiModelProperty(value = "经办人id")
    private Long assigneeId;
    @ApiModelProperty(value = "优先级id")
    private Long priorityId;
    @ApiModelProperty(value = "优先级名称")
    private String priorityName;
    @ApiModelProperty(value = "优先级颜色")
    private String priorityColour;
    @ApiModelProperty(value = "问题类型id")
    private Long issueTypeId;
    @ApiModelProperty(value = "问题类型名称")
    private String typeName;
    @ApiModelProperty(value = "状态id")
    private Long statusId;
    @ApiModelProperty(value = "状态名称")
    private String statusName;
    @ApiModelProperty(value = "状态编码")
    private String statusCode;
    @ApiModelProperty(value = "父级问题id")
    private Long parentIssueId;
    @ApiModelProperty(value = "关联的问题id")
    private Long relateIssueId;
    @ApiModelProperty(value = "预计结束时间")
    private Date estimatedEndTime;
    @ApiModelProperty(value = "子级")
    private List<IssueDailyWorkVO> childIssues;

    public Long getIssueId() {
        return issueId;
    }

    public void setIssueId(Long issueId) {
        this.issueId = issueId;
    }

    public String getSummary() {
        return summary;
    }

    public void setSummary(String summary) {
        this.summary = summary;
    }

    public Long getProjectId() {
        return projectId;
    }

    public void setProjectId(Long projectId) {
        this.projectId = projectId;
    }

    public String getTypeCode() {
        return typeCode;
    }

    public void setTypeCode(String typeCode) {
        this.typeCode = typeCode;
    }

    public String getIssueNum() {
        return issueNum;
    }

    public void setIssueNum(String issueNum) {
        this.issueNum = issueNum;
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

    public Long getPriorityId() {
        return priorityId;
    }

    public void setPriorityId(Long priorityId) {
        this.priorityId = priorityId;
    }

    public String getPriorityName() {
        return priorityName;
    }

    public void setPriorityName(String priorityName) {
        this.priorityName = priorityName;
    }

    public String getPriorityColour() {
        return priorityColour;
    }

    public void setPriorityColour(String priorityColour) {
        this.priorityColour = priorityColour;
    }

    public Long getIssueTypeId() {
        return issueTypeId;
    }

    public void setIssueTypeId(Long issueTypeId) {
        this.issueTypeId = issueTypeId;
    }

    public String getTypeName() {
        return typeName;
    }

    public void setTypeName(String typeName) {
        this.typeName = typeName;
    }

    public Long getStatusId() {
        return statusId;
    }

    public void setStatusId(Long statusId) {
        this.statusId = statusId;
    }

    public String getStatusName() {
        return statusName;
    }

    public void setStatusName(String statusName) {
        this.statusName = statusName;
    }

    public String getStatusCode() {
        return statusCode;
    }

    public void setStatusCode(String statusCode) {
        this.statusCode = statusCode;
    }

    public Long getParentIssueId() {
        return parentIssueId;
    }

    public void setParentIssueId(Long parentIssueId) {
        this.parentIssueId = parentIssueId;
    }

    public Long getRelateIssueId() {
        return relateIssueId;
    }

    public void setRelateIssueId(Long relateIssueId) {
        this.relateIssueId = relateIssueId;
    }

    public Date getEstimatedEndTime() {
        return estimatedEndTime;
    }

    public void setEstimatedEndTime(Date estimatedEndTime) {
        this.estimatedEndTime = estimatedEndTime;
    }

    public List<IssueDailyWorkVO> getChildIssues() {
        return childIssues;
    }

    public void setChildIssues(List<IssueDailyWorkVO> childIssues) {
        this.childIssues = childIssues;
    }
}
