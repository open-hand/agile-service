package io.choerodon.agile.api.vo;

import io.choerodon.agile.api.vo.business.IssueSearchVO;
import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

import java.math.BigDecimal;
import java.util.Date;
import java.util.List;

/**
 * Created by jian_zhang02@163.com on 2018/5/16.
 */
public class SprintSearchVO {
    @Encrypt(ignoreValue = {"0"})
    @ApiModelProperty(value = "冲刺id")
    private Long sprintId;
    @ApiModelProperty(value = "冲刺名称")
    private String sprintName;
    @ApiModelProperty(value = "冲刺目标")
    private String sprintGoal;
    @ApiModelProperty(value = "开始时间")
    private Date startDate;
    @ApiModelProperty(value = "结束时间")
    private Date endDate;
    @ApiModelProperty(value = "状态编码")
    private String statusCode;
    @ApiModelProperty(value = "问题数量")
    private Integer issueCount;
    @ApiModelProperty(value = "待处理故事点")
    private BigDecimal todoStoryPoint;
    @ApiModelProperty(value = "处理中故事点")
    private BigDecimal doingStoryPoint;
    @ApiModelProperty(value = "已完成故事点")
    private BigDecimal doneStoryPoint;
    @ApiModelProperty(value = "问题搜索条件")
    private List<IssueSearchVO> issueSearchVOList;
    @ApiModelProperty(value = "分配的问题")
    private List<AssigneeIssueVO> assigneeIssues;
    @ApiModelProperty(value = "乐观锁")
    private String objectVersionNumber;
    @ApiModelProperty(value = "状态编码")
    private List<String> statusCodes;

    public Long getSprintId() {
        return sprintId;
    }

    public void setSprintId(Long sprintId) {
        this.sprintId = sprintId;
    }

    public String getSprintName() {
        return sprintName;
    }

    public void setSprintName(String sprintName) {
        this.sprintName = sprintName;
    }

    public String getSprintGoal() {
        return sprintGoal;
    }

    public void setSprintGoal(String sprintGoal) {
        this.sprintGoal = sprintGoal;
    }

    public Date getStartDate() {
        return startDate;
    }

    public void setStartDate(Date startDate) {
        this.startDate = startDate;
    }

    public Date getEndDate() {
        return endDate;
    }

    public void setEndDate(Date endDate) {
        this.endDate = endDate;
    }

    public String getStatusCode() {
        return statusCode;
    }

    public void setStatusCode(String statusCode) {
        this.statusCode = statusCode;
    }

    public Integer getIssueCount() {
        return issueCount;
    }

    public void setIssueCount(Integer issueCount) {
        this.issueCount = issueCount;
    }

    public List<IssueSearchVO> getIssueSearchVOList() {
        return issueSearchVOList;
    }

    public void setIssueSearchVOList(List<IssueSearchVO> issueSearchVOList) {
        this.issueSearchVOList = issueSearchVOList;
    }

    public List<AssigneeIssueVO> getAssigneeIssues() {
        return assigneeIssues;
    }

    public void setAssigneeIssues(List<AssigneeIssueVO> assigneeIssues) {
        this.assigneeIssues = assigneeIssues;
    }

    public String getObjectVersionNumber() {
        return objectVersionNumber;
    }

    public void setObjectVersionNumber(String objectVersionNumber) {
        this.objectVersionNumber = objectVersionNumber;
    }

    public BigDecimal getTodoStoryPoint() {
        return todoStoryPoint;
    }

    public void setTodoStoryPoint(BigDecimal todoStoryPoint) {
        this.todoStoryPoint = todoStoryPoint;
    }

    public BigDecimal getDoingStoryPoint() {
        return doingStoryPoint;
    }

    public void setDoingStoryPoint(BigDecimal doingStoryPoint) {
        this.doingStoryPoint = doingStoryPoint;
    }

    public BigDecimal getDoneStoryPoint() {
        return doneStoryPoint;
    }

    public void setDoneStoryPoint(BigDecimal doneStoryPoint) {
        this.doneStoryPoint = doneStoryPoint;
    }

    public List<String> getStatusCodes() {
        return statusCodes;
    }

    public void setStatusCodes(List<String> statusCodes) {
        this.statusCodes = statusCodes;
    }
}
