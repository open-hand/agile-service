package io.choerodon.agile.api.vo;

import io.choerodon.agile.infra.dto.IssueSprintDTO;
import io.choerodon.agile.infra.dto.UserMessageDTO;
import org.hzero.starter.keyencrypt.core.Encrypt;

import java.util.Date;
import java.util.List;

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

    private List<GanttChartVO> children;

    private Boolean completed;

    private Date actualCompletedDate;

    private IssueSprintDTO sprint;

    private IssueEpicVO epic;

    private FeatureForIssueVO feature;

    public IssueEpicVO getEpic() {
        return epic;
    }

    public void setEpic(IssueEpicVO epic) {
        this.epic = epic;
    }

    public FeatureForIssueVO getFeature() {
        return feature;
    }

    public void setFeature(FeatureForIssueVO feature) {
        this.feature = feature;
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

    public List<GanttChartVO> getChildren() {
        return children;
    }

    public void setChildren(List<GanttChartVO> children) {
        this.children = children;
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
}
