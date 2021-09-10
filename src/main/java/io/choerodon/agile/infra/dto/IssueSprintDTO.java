package io.choerodon.agile.infra.dto;

import io.choerodon.agile.infra.utils.StringUtil;
import org.hzero.starter.keyencrypt.core.Encrypt;

import java.util.Date;

/**
 * @author dinghuang123@gmail.com
 * @since 2018/8/20
 */
public class IssueSprintDTO {

    private String sprintName;

    private String statusCode;

    @Encrypt
    private Long sprintId;

    private Date startDate;

    private Date endDate;

    private Date actualEndDate;
    @Encrypt
    private Long issueId;

    public Long getIssueId() {
        return issueId;
    }

    public void setIssueId(Long issueId) {
        this.issueId = issueId;
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

    public Date getActualEndDate() {
        return actualEndDate;
    }

    public void setActualEndDate(Date actualEndDate) {
        this.actualEndDate = actualEndDate;
    }

    public String getSprintName() {
        return sprintName;
    }

    public void setSprintName(String sprintName) {
        this.sprintName = sprintName;
    }

    public String getStatusCode() {
        return statusCode;
    }

    public void setStatusCode(String statusCode) {
        this.statusCode = statusCode;
    }

    public Long getSprintId() {
        return sprintId;
    }

    public void setSprintId(Long sprintId) {
        this.sprintId = sprintId;
    }

    @Override
    public String toString() {
        return StringUtil.getToString(this);
    }
}
