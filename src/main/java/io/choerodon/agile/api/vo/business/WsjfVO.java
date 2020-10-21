package io.choerodon.agile.api.vo.business;

import io.choerodon.agile.api.vo.StatusVO;
import org.hzero.starter.keyencrypt.core.Encrypt;

import java.math.BigDecimal;

/**
 * @author zhaotianxin
 * @since 2020/2/10
 */
public class WsjfVO {

    @Encrypt
    private Long id;

    @Encrypt
    private Long issueId;

    private String issueName;

    private String piName;

    private Long projectId;

    private Long userBusinessValue;

    private Long timeCriticality;

    private Long rrOeValue;

    private Long jobSize;

    private Long costDelay;

    private BigDecimal wsjf;

    private StatusVO statusVO;

    private Long objectVersionNumber;

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public Long getIssueId() {
        return issueId;
    }

    public void setIssueId(Long issueId) {
        this.issueId = issueId;
    }

    public String getIssueName() {
        return issueName;
    }

    public void setIssueName(String issueName) {
        this.issueName = issueName;
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

    public Long getRrOeValue() {
        return rrOeValue;
    }

    public void setRrOeValue(Long rrOeValue) {
        this.rrOeValue = rrOeValue;
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

    public BigDecimal getWsjf() {
        return wsjf;
    }

    public void setWsjf(BigDecimal wsjf) {
        this.wsjf = wsjf;
    }

    public StatusVO getStatusVO() {
        return statusVO;
    }

    public void setStatusVO(StatusVO statusVO) {
        this.statusVO = statusVO;
    }

    public String getPiName() {
        return piName;
    }

    public void setPiName(String piName) {
        this.piName = piName;
    }

    public Long getObjectVersionNumber() {
        return objectVersionNumber;
    }

    public void setObjectVersionNumber(Long objectVersionNumber) {
        this.objectVersionNumber = objectVersionNumber;
    }
}
