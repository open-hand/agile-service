package io.choerodon.agile.api.vo;

import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

public class SprintBugVO {
    @ApiModelProperty(value = "冲刺")
    @Encrypt
    private Long sprintId;
    @ApiModelProperty(value = "冲刺名称")
    private String sprintName;
    @ApiModelProperty(value = "主要负责人")
    @Encrypt
    private Long responsibleId;
    @ApiModelProperty(value = "经办人")
    private String name;
    @ApiModelProperty(value = "经办人登陆名")
    private String loginName;
    @ApiModelProperty(value = "经办人真实名字")
    private String realName;
    @ApiModelProperty(value = "头像")
    private String imageUrl;
    @ApiModelProperty(value = "bug数量")
    private Integer bugCount;

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

    public Long getResponsibleId() {
        return responsibleId;
    }

    public void setResponsibleId(Long responsibleId) {
        this.responsibleId = responsibleId;
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

    public String getImageUrl() {
        return imageUrl;
    }

    public void setImageUrl(String imageUrl) {
        this.imageUrl = imageUrl;
    }

    public Integer getBugCount() {
        return bugCount;
    }

    public void setBugCount(Integer bugCount) {
        this.bugCount = bugCount;
    }
}
