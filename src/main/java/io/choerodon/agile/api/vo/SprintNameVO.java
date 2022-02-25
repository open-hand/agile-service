package io.choerodon.agile.api.vo;


import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

import java.util.Date;

/**
 * Created by jian_zhang02@163.com on 2018/5/17.
 */
public class SprintNameVO {

    @ApiModelProperty(value = "冲刺id")
    @Encrypt
    private Long sprintId;

    @ApiModelProperty(value = "冲刺名称")
    private String sprintName;

    @ApiModelProperty(value = "冲刺开始时间")
    private Date startDate;

    @ApiModelProperty(value = "冲刺结束时间")
    private Date endDate;

    @ApiModelProperty(value = "冲刺状态")
    private String statusCode;

    @ApiModelProperty(value = "冲刺实际结束时间")
    private Date actualEndDate;
    @Encrypt
    private Long parentId;

    public Long getParentId() {
        return parentId;
    }

    public void setParentId(Long parentId) {
        this.parentId = parentId;
    }

    public Long getSprintId() {
        return sprintId;
    }

    public void setSprintId(Long sprintId) {
        this.sprintId = sprintId;
    }

    public String getSprintName() {
        return sprintName;
    }

    public void setSprintName(String sprintName) {
        this.sprintName = sprintName;
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

    public void setStatusCode(String statusCode) {
        this.statusCode = statusCode;
    }

    public String getStatusCode() {
        return statusCode;
    }

    public Date getActualEndDate() {
        return actualEndDate;
    }

    public void setActualEndDate(Date actualEndDate) {
        this.actualEndDate = actualEndDate;
    }
}
