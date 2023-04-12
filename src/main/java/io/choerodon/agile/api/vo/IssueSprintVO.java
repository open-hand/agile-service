package io.choerodon.agile.api.vo;


import java.io.Serializable;
import java.util.Date;

import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.annotations.ApiModelProperty;

import io.choerodon.agile.infra.utils.StringUtil;

import org.hzero.starter.keyencrypt.core.Encrypt;

/**
 * @author dinghuang123@gmail.com
 * @since 2018/11/7
 */
@JsonInclude(JsonInclude.Include.NON_NULL)
public class IssueSprintVO implements Serializable {

    @ApiModelProperty(value = "冲刺名称")
    private String sprintName;

    @ApiModelProperty(value = "状态code")
    private String statusCode;

    @ApiModelProperty(value = "冲刺id")
    @Encrypt
    private Long sprintId;

    @Encrypt
    @ApiModelProperty(value = "问题id")
    private Long issueId;

    private Date startDate;

    private Date endDate;

    private Date actualEndDate;

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
}
