package io.choerodon.agile.api.vo;

import io.swagger.annotations.ApiModelProperty;

import java.math.BigDecimal;

public class SprintStoryPointVO {
    @ApiModelProperty(value = "主要负责人")
    private Long mainResponsibleId;
    @ApiModelProperty(value = "经办人")
    private String name;
    @ApiModelProperty(value = "经办人登陆名")
    private String loginName;
    @ApiModelProperty(value = "经办人真实名字")
    private String realName;
    @ApiModelProperty(value = "计划故事点")
    private BigDecimal storyPoints;
    @ApiModelProperty(value = "完成故事点")
    private BigDecimal storyPointsComplete;
    @ApiModelProperty(value = "主要负责人故事点占比")
    private BigDecimal mainStoryPointsRate;
    @ApiModelProperty(value = "主要负责人完成故事点占比")
    private BigDecimal mainStoryPointsCompleteRate;

    public Long getMainResponsibleId() {
        return mainResponsibleId;
    }

    public void setMainResponsibleId(Long mainResponsibleId) {
        this.mainResponsibleId = mainResponsibleId;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getLoginName() {
        return loginName;
    }

    public void setLoginName(String loginName) {
        this.loginName = loginName;
    }

    public String getRealName() {
        return realName;
    }

    public void setRealName(String realName) {
        this.realName = realName;
    }

    public BigDecimal getStoryPoints() {
        return storyPoints;
    }

    public void setStoryPoints(BigDecimal storyPoints) {
        this.storyPoints = storyPoints;
    }

    public BigDecimal getStoryPointsComplete() {
        return storyPointsComplete;
    }

    public void setStoryPointsComplete(BigDecimal storyPointsComplete) {
        this.storyPointsComplete = storyPointsComplete;
    }

    public BigDecimal getMainStoryPointsRate() {
        return mainStoryPointsRate;
    }

    public void setMainStoryPointsRate(BigDecimal mainStoryPointsRate) {
        this.mainStoryPointsRate = mainStoryPointsRate;
    }

    public BigDecimal getMainStoryPointsCompleteRate() {
        return mainStoryPointsCompleteRate;
    }

    public void setMainStoryPointsCompleteRate(BigDecimal mainStoryPointsCompleteRate) {
        this.mainStoryPointsCompleteRate = mainStoryPointsCompleteRate;
    }
}
