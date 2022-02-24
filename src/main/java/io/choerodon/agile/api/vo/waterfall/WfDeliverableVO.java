package io.choerodon.agile.api.vo.waterfall;

import javax.validation.constraints.NotNull;

import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

/**
 * @author scp
 * @since 2022/2/18
 */
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
}
