package io.choerodon.agile.api.vo.waterfall;

import java.util.Date;
import java.util.List;
import javax.validation.constraints.NotNull;

import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.annotations.ApiModelProperty;

import org.hzero.starter.keyencrypt.core.Encrypt;

/**
 * @author scp
 * @since 2022/2/18
 */
@JsonInclude(JsonInclude.Include.NON_NULL)
public class WfDeliverableVO {
    @ApiModelProperty(value = "交付物名称")
    @NotNull
    private String name;
    @ApiModelProperty(value = "交付物类型")
    @NotNull
    private String type;
    @Encrypt
    @ApiModelProperty(value = "里程碑id")
    private Long milestoneId;
    // 查询使用
    @ApiModelProperty(value = "里程碑名称")
    private String milestoneName;
    @ApiModelProperty(value = "里程碑状态")
    private String milestoneStatus;
    @ApiModelProperty(value = "实际交付物")
    private String actualDeliverable;
    @ApiModelProperty(value = "提交人")
    private String userName;
    @ApiModelProperty(value = "提交日期")
    private Date submitDate;
    @ApiModelProperty(value = "开始日期")
    private Date startDate;
    @ApiModelProperty(value = "结束日期")
    private Date endDate;
    @Encrypt
    @ApiModelProperty(value = "提交人Id")
    private List<Long> userIds;
    @Encrypt
    @ApiModelProperty(value = "里程碑状态id")
    private Long milestoneStatusId;
    @ApiModelProperty(value = "模糊搜索")
    private String params;

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    public Long getMilestoneId() {
        return milestoneId;
    }

    public void setMilestoneId(Long milestoneId) {
        this.milestoneId = milestoneId;
    }

    public String getActualDeliverable() {
        return actualDeliverable;
    }

    public void setActualDeliverable(String actualDeliverable) {
        this.actualDeliverable = actualDeliverable;
    }

    public String getUserName() {
        return userName;
    }

    public void setUserName(String userName) {
        this.userName = userName;
    }

    public String getMilestoneName() {
        return milestoneName;
    }

    public void setMilestoneName(String milestoneName) {
        this.milestoneName = milestoneName;
    }

    public String getMilestoneStatus() {
        return milestoneStatus;
    }

    public void setMilestoneStatus(String milestoneStatus) {
        this.milestoneStatus = milestoneStatus;
    }

    public Long getMilestoneStatusId() {
        return milestoneStatusId;
    }

    public void setMilestoneStatusId(Long milestoneStatusId) {
        this.milestoneStatusId = milestoneStatusId;
    }

    public Date getSubmitDate() {
        return submitDate;
    }

    public void setSubmitDate(Date submitDate) {
        this.submitDate = submitDate;
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

    public String getParams() {
        return params;
    }

    public void setParams(String params) {
        this.params = params;
    }

    public List<Long> getUserIds() {
        return userIds;
    }

    public void setUserIds(List<Long> userIds) {
        this.userIds = userIds;
    }
}
