package io.choerodon.agile.api.vo;

import io.choerodon.agile.infra.dto.UserMessageDTO;
import org.hzero.starter.keyencrypt.core.Encrypt;

import java.math.BigDecimal;
import java.util.Date;

/**
 * @author zhaotianxin
 * @date 2021-10-15 13:37
 */
public class WorkHoursLogVO {

    @Encrypt
    private Long userId;

    @Encrypt
    private Long issueTypeId;

    private String issueNum;

    private String summary;

    @Encrypt
    private String issueId;

    private String objectVersionNumber;

    private Long projectId;

    @Encrypt
    private Long statusId;

    private StatusVO statusVO;

    private IssueTypeVO issueTypeVO;

    private UserMessageDTO user;

    private BigDecimal workTime;

    private Date startDate;

    private ProjectVO projectVO;

    public Long getUserId() {
        return userId;
    }

    public void setUserId(Long userId) {
        this.userId = userId;
    }

    public Long getIssueTypeId() {
        return issueTypeId;
    }

    public void setIssueTypeId(Long issueTypeId) {
        this.issueTypeId = issueTypeId;
    }

    public String getIssueNum() {
        return issueNum;
    }

    public void setIssueNum(String issueNum) {
        this.issueNum = issueNum;
    }

    public String getSummary() {
        return summary;
    }

    public void setSummary(String summary) {
        this.summary = summary;
    }

    public String getIssueId() {
        return issueId;
    }

    public void setIssueId(String issueId) {
        this.issueId = issueId;
    }

    public String getObjectVersionNumber() {
        return objectVersionNumber;
    }

    public void setObjectVersionNumber(String objectVersionNumber) {
        this.objectVersionNumber = objectVersionNumber;
    }

    public Long getProjectId() {
        return projectId;
    }

    public void setProjectId(Long projectId) {
        this.projectId = projectId;
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

    public IssueTypeVO getIssueTypeVO() {
        return issueTypeVO;
    }

    public void setIssueTypeVO(IssueTypeVO issueTypeVO) {
        this.issueTypeVO = issueTypeVO;
    }

    public UserMessageDTO getUser() {
        return user;
    }

    public void setUser(UserMessageDTO user) {
        this.user = user;
    }

    public BigDecimal getWorkTime() {
        return workTime;
    }

    public void setWorkTime(BigDecimal workTime) {
        this.workTime = workTime;
    }

    public ProjectVO getProjectVO() {
        return projectVO;
    }

    public void setProjectVO(ProjectVO projectVO) {
        this.projectVO = projectVO;
    }

    public Date getStartDate() {
        return startDate;
    }

    public void setStartDate(Date startDate) {
        this.startDate = startDate;
    }
}
