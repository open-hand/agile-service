package io.choerodon.agile.api.vo.business;

import io.choerodon.agile.api.vo.StatusVO;
import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

import java.math.BigDecimal;

/**
 * @author zhaotianxin
 * @since 2020/2/10
 */
public class WsjfVO {

    @Encrypt
    @ApiModelProperty(value = "主键")
    private Long id;

    @Encrypt
    @ApiModelProperty(value = "问题id")
    private Long issueId;
    @ApiModelProperty(value = "问题名称")
    private String issueName;
    @ApiModelProperty(value = "pi名称")
    private String piName;
    @ApiModelProperty(value = "项目id")
    private Long projectId;
    @ApiModelProperty(value = "用户/业务价值")
    private Long userBusinessValue;
    @ApiModelProperty(value = "时间紧迫性")
    private Long timeCriticality;
    @ApiModelProperty(value = "产品状态id")
    private Long rrOeValue;
    @ApiModelProperty(value = "工作规模")
    private Long jobSize;
    @ApiModelProperty(value = "延迟成本")
    private Long costDelay;
    @ApiModelProperty(value = "wsjf")
    private BigDecimal wsjf;
    @ApiModelProperty(value = "状态")
    private StatusVO statusVO;
    @ApiModelProperty(value = "产品状态id")
    private Long objectVersionNumber;

    public Long getProjectId() {
        return projectId;
    }

    public void setProjectId(Long projectId) {
        this.projectId = projectId;
    }

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
