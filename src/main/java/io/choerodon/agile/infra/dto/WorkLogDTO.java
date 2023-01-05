package io.choerodon.agile.infra.dto;

import java.math.BigDecimal;
import java.util.Date;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.Table;

import io.choerodon.mybatis.annotation.ModifyAudit;
import io.choerodon.mybatis.annotation.VersionAudit;
import io.choerodon.mybatis.domain.AuditDomain;

/**
 * Created by HuangFuqiang@choerodon.io on 2018/5/18.
 * Email: fuqianghuang01@gmail.com
 */
@Table(name = "agile_work_log")
@ModifyAudit
@VersionAudit
public class WorkLogDTO extends AuditDomain {

    public static final String FIELD_ISSUE_ID = "issueId";
    public static final String FIELD_PROJECT_ID = "projectId";

    @Id
    @GeneratedValue
    private Long logId;

    private BigDecimal workTime;

    private Date startDate;

    private String description;

    private Long issueId;

    private Long projectId;

    public Long getLogId() {
        return logId;
    }

    public void setLogId(Long logId) {
        this.logId = logId;
    }

    public void setWorkTime(BigDecimal workTime) {
        this.workTime = workTime;
    }

    public BigDecimal getWorkTime() {
        return workTime;
    }

    public void setStartDate(Date startDate) {
        this.startDate = startDate;
    }

    public Date getStartDate() {
        return startDate;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public Long getIssueId() {
        return issueId;
    }

    public void setIssueId(Long issueId) {
        this.issueId = issueId;
    }

    public Long getProjectId() {
        return projectId;
    }

    public void setProjectId(Long projectId) {
        this.projectId = projectId;
    }
}
