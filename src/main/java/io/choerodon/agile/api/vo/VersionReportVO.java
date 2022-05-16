package io.choerodon.agile.api.vo;

import io.swagger.annotations.ApiModelProperty;

import java.util.Date;
import java.util.List;

/**
 * Created by jian_zhang02@163.com on 2018/7/5.
 */
public class VersionReportVO {
    @ApiModelProperty(value = "总字段")
    private Integer totalField;
    @ApiModelProperty(value = "完成的字段")
    private Integer completedField;
    @ApiModelProperty(value = "未预期进度")
    private double  unEstimatedPercentage;
    @ApiModelProperty(value = "修改日期")
    private Date changeDate;
    @ApiModelProperty(value = "字段改变问题")
    private List<IssueChangeVO> fieldChangIssues;
    @ApiModelProperty(value = "完成的问题")
    private List<IssueChangeVO> completedIssues;
    @ApiModelProperty(value = "未完成的问题")
    private List<IssueChangeVO> unCompletedIssues;
    @ApiModelProperty(value = "添加的问题")
    private List<IssueChangeVO> addIssues;
    @ApiModelProperty(value = "移除的问题")
    private List<IssueChangeVO> removeIssues;

    public Integer getTotalField() {
        return totalField;
    }

    public void setTotalField(Integer totalField) {
        this.totalField = totalField;
    }

    public Integer getCompletedField() {
        return completedField;
    }

    public void setCompletedField(Integer completedField) {
        this.completedField = completedField;
    }

    public double getUnEstimatedPercentage() {
        return unEstimatedPercentage;
    }

    public void setUnEstimatedPercentage(double unEstimatedPercentage) {
        this.unEstimatedPercentage = unEstimatedPercentage;
    }

    public Date getChangeDate() {
        return changeDate;
    }

    public void setChangeDate(Date changeDate) {
        this.changeDate = changeDate;
    }

    public List<IssueChangeVO> getFieldChangIssues() {
        return fieldChangIssues;
    }

    public void setFieldChangIssues(List<IssueChangeVO> fieldChangIssues) {
        this.fieldChangIssues = fieldChangIssues;
    }

    public List<IssueChangeVO> getCompletedIssues() {
        return completedIssues;
    }

    public void setCompletedIssues(List<IssueChangeVO> completedIssues) {
        this.completedIssues = completedIssues;
    }

    public List<IssueChangeVO> getUnCompletedIssues() {
        return unCompletedIssues;
    }

    public void setUnCompletedIssues(List<IssueChangeVO> unCompletedIssues) {
        this.unCompletedIssues = unCompletedIssues;
    }

    public List<IssueChangeVO> getAddIssues() {
        return addIssues;
    }

    public void setAddIssues(List<IssueChangeVO> addIssues) {
        this.addIssues = addIssues;
    }

    public List<IssueChangeVO> getRemoveIssues() {
        return removeIssues;
    }

    public void setRemoveIssues(List<IssueChangeVO> removeIssues) {
        this.removeIssues = removeIssues;
    }

}
