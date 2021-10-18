package io.choerodon.agile.api.vo;

import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

import java.util.Date;
import java.util.List;

/**
 * @author zhaotianxin
 * @date 2021-10-11 14:00
 */
public class WorkItemSearchVO {
    @ApiModelProperty(value = "按项目筛选")
    private List<Long> projectIds;

    @ApiModelProperty(value = "按概要筛选")
    private String params;

    @ApiModelProperty(value = "按人员筛选 （assignee/partner）")
    private List<String> assigneeFilter;

    @ApiModelProperty(value = "开始时间")
    private Date startTime;

    @ApiModelProperty(value = "结束时间")
    private Date endTime;

    @Encrypt
    @ApiModelProperty(value = "按父任务id查询")
    private Long filterIssueId;

    @ApiModelProperty("生成日历文件时只有开始时间")
    private Boolean onlyStartTime;

    public List<Long> getProjectIds() {
        return projectIds;
    }

    public void setProjectIds(List<Long> projectIds) {
        this.projectIds = projectIds;
    }

    public String getParams() {
        return params;
    }

    public void setParams(String params) {
        this.params = params;
    }

    public List<String> getAssigneeFilter() {
        return assigneeFilter;
    }

    public void setAssigneeFilter(List<String> assigneeFilter) {
        this.assigneeFilter = assigneeFilter;
    }

    public Date getStartTime() {
        return startTime;
    }

    public void setStartTime(Date startTime) {
        this.startTime = startTime;
    }

    public Date getEndTime() {
        return endTime;
    }

    public void setEndTime(Date endTime) {
        this.endTime = endTime;
    }

    public Long getFilterIssueId() {
        return filterIssueId;
    }

    public void setFilterIssueId(Long filterIssueId) {
        this.filterIssueId = filterIssueId;
    }

    public Boolean getOnlyStartTime() {
        return onlyStartTime;
    }

    public void setOnlyStartTime(Boolean onlyStartTime) {
        this.onlyStartTime = onlyStartTime;
    }
}
