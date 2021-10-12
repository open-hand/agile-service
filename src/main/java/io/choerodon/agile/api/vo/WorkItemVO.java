package io.choerodon.agile.api.vo;

import io.choerodon.agile.infra.dto.UserMessageDTO;
import org.hzero.starter.keyencrypt.core.Encrypt;

import java.util.Date;

/**
 * @author zhaotianxin
 * @date 2021-10-11 13:56
 */
public class WorkItemVO {
    @Encrypt
    private Long issueId;

    private String summary;

    private Date estimatedStartTime;

    @Encrypt
    private Long priorityId;

    @Encrypt
    private Long statusId;

    private StatusVO statusVO;

    private PriorityVO priorityVO;

    @Encrypt
    private Long assigneeId;

    private Date estimatedEndTime;

    private Long projectId;

    private CountVO countVO;

    private UserMessageDTO assignee;

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
}
