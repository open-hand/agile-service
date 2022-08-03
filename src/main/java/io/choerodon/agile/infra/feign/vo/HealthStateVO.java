package io.choerodon.agile.infra.feign.vo;

import io.swagger.annotations.ApiModelProperty;

/**
 * Copyright (c) 2022. Hand Enterprise Solution Company. All right reserved.
 *
 * @author zongqi.hao@zknow.com
 * @since 2022/8/1
 */
public class HealthStateVO {

    @ApiModelProperty(value = "名称")
    private String name;

    @ApiModelProperty(value = "编码")
    private String code;

    @ApiModelProperty(value = "颜色")
    private String color;

    @ApiModelProperty(value = "描述")
    private String description;

    @ApiModelProperty(value = "是否为新建项目的默认健康状态。1是，0否,默认否")
    private Boolean defaultFlag;

    @ApiModelProperty(value = "是否启用。1启用，0未启用, 默认启用")
    private Boolean enabledFlag;

    @ApiModelProperty(value = "拖动排序字段")
    private String rank;


    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getCode() {
        return code;
    }

    public void setCode(String code) {
        this.code = code;
    }

    public String getColor() {
        return color;
    }

    public void setColor(String color) {
        this.color = color;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public Boolean getDefaultFlag() {
        return defaultFlag;
    }

    public void setDefaultFlag(Boolean defaultFlag) {
        this.defaultFlag = defaultFlag;
    }

    public Boolean getEnabledFlag() {
        return enabledFlag;
    }

    public void setEnabledFlag(Boolean enabledFlag) {
        this.enabledFlag = enabledFlag;
    }

    public String getRank() {
        return rank;
    }

    public void setRank(String rank) {
        this.rank = rank;
    }
}
