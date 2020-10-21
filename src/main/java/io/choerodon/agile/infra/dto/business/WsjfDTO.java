package io.choerodon.agile.infra.dto.business;

import io.choerodon.mybatis.domain.AuditDomain;
import org.hzero.starter.keyencrypt.core.Encrypt;

import java.math.BigDecimal;

/**
 * @author zhaotianxin
 * @since 2020/2/10
 */
public class WsjfDTO extends AuditDomain {
    @Encrypt
    private Long id;

    @Encrypt
    private Long issueId;

    private Long projectId;

    private Long userBusinessValue;

    private Long timeCriticality;

    private Long rrOeValue;

    private Long jobSize;

    private Long costDelay;

    private BigDecimal wsjf;

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public Long getRrOeValue() {
        return rrOeValue;
    }

    public void setRrOeValue(Long rrOeValue) {
        this.rrOeValue = rrOeValue;
    }

    public BigDecimal getWsjf() {
        return wsjf;
    }

    public void setWsjf(BigDecimal wsjf) {
        this.wsjf = wsjf;
    }

    public Long getProjectId() {
        return projectId;
    }

    public void setProjectId(Long projectId) {
        this.projectId = projectId;
    }

    public Long getIssueId() {
        return issueId;
    }

    public void setIssueId(Long issueId) {
        this.issueId = issueId;
    }

    public Long getUserBusinessValue() {
        return userBusinessValue;
    }

    public void setUserBusinessValue(Long userBusinessValue) {
        this.userBusinessValue = userBusinessValue;
    }

    public Long getTimeCriticality() {
        return timeCriticality;
    }

    public void setTimeCriticality(Long timeCriticality) {
        this.timeCriticality = timeCriticality;
    }

    public Long getJobSize() {
        return jobSize;
    }

    public void setJobSize(Long jobSize) {
        this.jobSize = jobSize;
    }

    public Long getCostDelay() {
        return costDelay;
    }

    public void setCostDelay(Long costDelay) {
        this.costDelay = costDelay;
    }
}
