package io.choerodon.agile.infra.dto;

import io.choerodon.agile.infra.utils.StringUtil;
import io.choerodon.mybatis.annotation.ModifyAudit;
import io.choerodon.mybatis.annotation.VersionAudit;
import io.choerodon.mybatis.domain.AuditDomain;

import javax.persistence.GeneratedValue;

import javax.persistence.Id;
import javax.persistence.Table;

/**
 * @author dinghuang123@gmail.com
 * @since 2018/5/30
 */
@Table(name = "agile_project_info")
@ModifyAudit
@VersionAudit
public class ProjectInfoDTO extends AuditDomain {

    @Id
    @GeneratedValue
    private Long infoId;

    private Long projectId;

    private String projectCode;

    private Long issueMaxNum;

    private Long feedbackMaxNum;

    /**
     * 默认经办人
     */
    private Long defaultAssigneeId;

    /**
     * 经办人策略
     */
    private String defaultAssigneeType;

    /**
     * 经办人策略
     */
    private String defaultPriorityCode;

    /**
     * 是否隐藏历史迭代中已完成的子任务
     */
    private Boolean isHidePreSprintDoneSubissue;

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

    public void setFeedbackMaxNum(Long feedbackMaxNum) {
        this.feedbackMaxNum = feedbackMaxNum;
    }

    public Long getFeedbackMaxNum() {
        return feedbackMaxNum;
    }

    @Override
    public String toString() {
        return StringUtil.getToString(this);
    }

    public Boolean getHidePreSprintDoneSubissue() {
        return isHidePreSprintDoneSubissue;
    }

    public void setHidePreSprintDoneSubissue(Boolean hidePreSprintDoneSubissue) {
        isHidePreSprintDoneSubissue = hidePreSprintDoneSubissue;
    }
}
