package io.choerodon.agile.api.vo;

import io.choerodon.agile.infra.dto.IssueSprintDTO;
import io.choerodon.agile.infra.dto.UserMessageDTO;
import org.hzero.starter.keyencrypt.core.Encrypt;

import java.util.Date;

/**
 * @author superlee
 * @since 2020-11-25
 */
public class GanttChartVO {

    public static final String FIELD_ISSUE_ID ="issueId";
    public static final String FIELD_SUMMARY ="summary";
    public static final String FIELD_ASSIGNEE ="assignee";
    public static final String FIELD_ESTIMATED_START_TIME ="estimatedStartTime";
    public static final String FIELD_ESTIMATED_END_TIME ="estimatedEndTime";
    public static final String FIELD_OBJECT_VERSION_NUMBER ="objectVersionNumber";
    public static final String FIELD_ISSUE_TYPE_VO ="issueTypeVO";
    public static final String FIELD_PARENT_ID ="parentId";

    @Encrypt
    private Long issueId;

    private String summary;

    private UserMessageDTO assignee;

    private Date estimatedStartTime;

    private Date estimatedEndTime;

    private Long objectVersionNumber;

    private IssueTypeVO issueTypeVO;

    @Encrypt
    private Long parentId;

    private StatusVO statusVO;

    private Boolean completed;

    private Date actualCompletedDate;

    private IssueSprintDTO sprint;

    private Date actualStartTime;

    private Date actualEndTime;

    private String epicName;

    private String color;

    private Long programId;

    private Long projectId;
    @Encrypt
    private Long featureId;
    @Encrypt
    private Long epicId;

    private String featureName;

    public String getFeatureName() {
        return featureName;
    }

    public void setFeatureName(String featureName) {
        this.featureName = featureName;
    }

    public Long getFeatureId() {
        return featureId;
    }

    public void setFeatureId(Long featureId) {
        this.featureId = featureId;
    }

    public Long getEpicId() {
        return epicId;
    }

    public void setEpicId(Long epicId) {
        this.epicId = epicId;
    }

    public String getEpicName() {
        return epicName;
    }

    public void setEpicName(String epicName) {
        this.epicName = epicName;
    }

    public String getColor() {
        return color;
    }

    public void setColor(String color) {
        this.color = color;
    }

    public Long getProgramId() {
        return programId;
    }

    public void setProgramId(Long programId) {
        this.programId = programId;
    }

    public Long getProjectId() {
        return projectId;
    }

    public void setProjectId(Long projectId) {
        this.projectId = projectId;
    }

    public IssueSprintDTO getSprint() {
        return sprint;
    }

    public void setSprint(IssueSprintDTO sprint) {
        this.sprint = sprint;
    }

    public Date getActualCompletedDate() {
        return actualCompletedDate;
    }

    public void setActualCompletedDate(Date actualCompletedDate) {
        this.actualCompletedDate = actualCompletedDate;
    }

    public Boolean getCompleted() {
        return completed;
    }

    public void setCompleted(Boolean completed) {
        this.completed = completed;
    }

    public StatusVO getStatusVO() {
        return statusVO;
    }

    public void setStatusVO(StatusVO statusVO) {
        this.statusVO = statusVO;
    }

    public Long getParentId() {
        return parentId;
    }

    public void setParentId(Long parentId) {
        this.parentId = parentId;
    }

    public IssueTypeVO getIssueTypeVO() {
        return issueTypeVO;
    }

    public void setIssueTypeVO(IssueTypeVO issueTypeVO) {
        this.issueTypeVO = issueTypeVO;
    }

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

    public UserMessageDTO getAssignee() {
        return assignee;
    }

    public void setAssignee(UserMessageDTO assignee) {
        this.assignee = assignee;
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

    public Long getObjectVersionNumber() {
        return objectVersionNumber;
    }

    public void setObjectVersionNumber(Long objectVersionNumber) {
        this.objectVersionNumber = objectVersionNumber;
    }

    public Date getActualStartTime() {
        return actualStartTime;
    }

    public void setActualStartTime(Date actualStartTime) {
        this.actualStartTime = actualStartTime;
    }

    public Date getActualEndTime() {
        return actualEndTime;
    }

    public void setActualEndTime(Date actualEndTime) {
        this.actualEndTime = actualEndTime;
    }
}
