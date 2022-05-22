package io.choerodon.agile.api.vo;

import io.choerodon.agile.infra.dto.UserMessageDTO;
import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

import java.util.Date;

/**
 * @author zhaotianxin
 * @date 2021-10-11 13:56
 */
public class WorkItemVO {
    @Encrypt
    @ApiModelProperty(value = "问题id")
    private Long issueId;
    @ApiModelProperty(value = "概要")
    private String summary;
    @ApiModelProperty(value = "预计开始时间")
    private Date estimatedStartTime;

    @Encrypt
    @ApiModelProperty(value = "优先级id")
    private Long priorityId;

    @Encrypt
    @ApiModelProperty(value = "状态id")
    private Long statusId;
    @ApiModelProperty(value = "状态")
    private StatusVO statusVO;
    @ApiModelProperty(value = "优先级")
    private PriorityVO priorityVO;

    @Encrypt
    @ApiModelProperty(value = "经办人id")
    private Long assigneeId;
    @ApiModelProperty(value = "预计结束时间")
    private Date estimatedEndTime;
    @ApiModelProperty(value = "项目id")
    private Long projectId;
    @ApiModelProperty(value = "统计数量")
    private CountVO countVO;
    @ApiModelProperty(value = "经办人")
    private UserMessageDTO assignee;
    @ApiModelProperty(value = "乐观锁")
    private Long objectVersionNumber;
    @ApiModelProperty(value = "问题编号")
    private String issueNum;

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

    public Date getEstimatedStartTime() {
        return estimatedStartTime;
    }

    public void setEstimatedStartTime(Date estimatedStartTime) {
        this.estimatedStartTime = estimatedStartTime;
    }

    public Date getEstimatedEndTime() {
        return estimatedEndTime;
    }

    public void setEstimatedEndTime(Date estimatedEndTime) {
        this.estimatedEndTime = estimatedEndTime;
    }

    public Long getProjectId() {
        return projectId;
    }

    public void setProjectId(Long projectId) {
        this.projectId = projectId;
    }

    public CountVO getCountVO() {
        return countVO;
    }

    public void setCountVO(CountVO countVO) {
        this.countVO = countVO;
    }

    public Long getPriorityId() {
        return priorityId;
    }

    public void setPriorityId(Long priorityId) {
        this.priorityId = priorityId;
    }

    public Long getAssigneeId() {
        return assigneeId;
    }

    public void setAssigneeId(Long assigneeId) {
        this.assigneeId = assigneeId;
    }

    public UserMessageDTO getAssignee() {
        return assignee;
    }

    public void setAssignee(UserMessageDTO assignee) {
        this.assignee = assignee;
    }

    public PriorityVO getPriorityVO() {
        return priorityVO;
    }

    public void setPriorityVO(PriorityVO priorityVO) {
        this.priorityVO = priorityVO;
    }

    public Long getStatusId() {
        return statusId;
    }

    public void setStatusId(Long statusId) {
        this.statusId = statusId;
    }

    public StatusVO getStatusVO() {
        return statusVO;
    }

    public void setStatusVO(StatusVO statusVO) {
        this.statusVO = statusVO;
    }

    public Long getObjectVersionNumber() {
        return objectVersionNumber;
    }

    public void setObjectVersionNumber(Long objectVersionNumber) {
        this.objectVersionNumber = objectVersionNumber;
    }

    public String getIssueNum() {
        return issueNum;
    }

    public void setIssueNum(String issueNum) {
        this.issueNum = issueNum;
    }
}
