package io.choerodon.agile.api.vo;

import java.util.Date;

/**
 * @author: 25499
 * @date: 2019/11/22 9:16
 * @description:
 */
public class ProjectInfoFixVO {
    private Long infoId;

    private Long projectId;

    private String projectCode;

    private Long issueMaxNum;

    private Long feedbackMaxNum;

    private Long defaultAssigneeId;

    private String defaultAssigneeType;

    private String defaultPriorityCode;

    private Long objectVersionNumber;

    private Long createdBy;

    private Date creationDate;

    private Long lastUpdatedBy;

    private Date lastUpdateDate;

    public Long getInfoId() {
        return infoId;
    }

    public void setInfoId(Long infoId) {
        this.infoId = infoId;
    }

    public Long getProjectId() {
        return projectId;
    }

    public void setProjectId(Long projectId) {
        this.projectId = projectId;
    }

    public String getProjectCode() {
        return projectCode;
    }

    public void setProjectCode(String projectCode) {
        this.projectCode = projectCode;
    }

    public Long getIssueMaxNum() {
        return issueMaxNum;
    }

    public void setIssueMaxNum(Long issueMaxNum) {
        this.issueMaxNum = issueMaxNum;
    }

    public Long getFeedbackMaxNum() {
        return feedbackMaxNum;
    }

    public void setFeedbackMaxNum(Long feedbackMaxNum) {
        this.feedbackMaxNum = feedbackMaxNum;
    }

    public Long getDefaultAssigneeId() {
        return defaultAssigneeId;
    }

    public void setDefaultAssigneeId(Long defaultAssigneeId) {
        this.defaultAssigneeId = defaultAssigneeId;
    }

    public String getDefaultAssigneeType() {
        return defaultAssigneeType;
    }

    public void setDefaultAssigneeType(String defaultAssigneeType) {
        this.defaultAssigneeType = defaultAssigneeType;
    }

    public String getDefaultPriorityCode() {
        return defaultPriorityCode;
    }

    public void setDefaultPriorityCode(String defaultPriorityCode) {
        this.defaultPriorityCode = defaultPriorityCode;
    }

    public Long getObjectVersionNumber() {
        return objectVersionNumber;
    }

    public void setObjectVersionNumber(Long objectVersionNumber) {
        this.objectVersionNumber = objectVersionNumber;
    }

    public Long getCreatedBy() {
        return createdBy;
    }

    public void setCreatedBy(Long createdBy) {
        this.createdBy = createdBy;
    }

    public Date getCreationDate() {
        return creationDate;
    }

    public void setCreationDate(Date creationDate) {
        this.creationDate = creationDate;
    }

    public Long getLastUpdatedBy() {
        return lastUpdatedBy;
    }

    public void setLastUpdatedBy(Long lastUpdatedBy) {
        this.lastUpdatedBy = lastUpdatedBy;
    }

    public Date getLastUpdateDate() {
        return lastUpdateDate;
    }

    public void setLastUpdateDate(Date lastUpdateDate) {
        this.lastUpdateDate = lastUpdateDate;
    }
}
