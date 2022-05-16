package io.choerodon.agile.api.vo;


import io.choerodon.agile.infra.utils.StringUtil;
import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;

/**
 * @author dinghuang123@gmail.com
 * @since 2018/6/20
 */
public class ReportIssueVO implements Serializable {
    @ApiModelProperty(value = "问题编号")
    private String issueNum;

    @Encrypt
    @ApiModelProperty(value = "问题id")
    private Long issueId;
    @ApiModelProperty(value = "日期")
    private Date date;
    @ApiModelProperty(value = "旧值")
    private BigDecimal oldValue;
    @ApiModelProperty(value = "类型")
    private String type;
    @ApiModelProperty(value = "新值")
    private BigDecimal newValue;
    @ApiModelProperty(value = "是否统计")
    private Boolean statistical;

    @Encrypt
    @ApiModelProperty(value = "父级问题id")
    private String parentIssueId;
    @ApiModelProperty(value = "父级问题编号")
    private String parentIssueNum;

    public Long getIssueId() {
        return issueId;
    }

    public void setIssueId(Long issueId) {
        this.issueId = issueId;
    }

    public Date getDate() {
        return date;
    }

    public void setDate(Date date) {
        this.date = date;
    }

    public String getIssueNum() {
        return issueNum;
    }

    public void setIssueNum(String issueNum) {
        this.issueNum = issueNum;
    }

    public void setOldValue(BigDecimal oldValue) {
        this.oldValue = oldValue;
    }

    public BigDecimal getOldValue() {
        return oldValue;
    }

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    public void setNewValue(BigDecimal newValue) {
        this.newValue = newValue;
    }

    public BigDecimal getNewValue() {
        return newValue;
    }

    public Boolean getStatistical() {
        return statistical;
    }

    public void setStatistical(Boolean statistical) {
        this.statistical = statistical;
    }

    public String getParentIssueId() {
        return parentIssueId;
    }

    public void setParentIssueId(String parentIssueId) {
        this.parentIssueId = parentIssueId;
    }

    public String getParentIssueNum() {
        return parentIssueNum;
    }

    public void setParentIssueNum(String parentIssueNum) {
        this.parentIssueNum = parentIssueNum;
    }

    @Override
    public String toString() {
        return StringUtil.getToString(this);
    }
}
