package io.choerodon.agile.api.vo;

import io.choerodon.agile.infra.dto.UserMessageDTO;
import io.swagger.annotations.ApiModelProperty;

import java.math.BigDecimal;
import java.util.Map;

public class SprintBugVO {
    @ApiModelProperty(value = "主要负责人")
    private Long mainResponsibleId;
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
