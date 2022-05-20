package io.choerodon.agile.api.vo;

import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

/**
 * @author zhaotianxin
 * @date 2021-03-25 10:50
 */
public class StatusTemplateVO {

    @Encrypt
    @ApiModelProperty(value = "状态id")
    private Long statusId;
    @ApiModelProperty(value = "名称")
    private String name;
    @ApiModelProperty(value = "类别编码")
    private String categoryCode;
    @ApiModelProperty(value = "位置")
    private Integer position;
    @ApiModelProperty(value = "模版是否完成")
    private Boolean templateCompleted;
    @ApiModelProperty(value = "组织id")
    private Long organizationId;

    public Long getStatusId() {
        return statusId;
    }

    public void setStatusId(Long statusId) {
        this.statusId = statusId;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getCategoryCode() {
        return categoryCode;
    }

    public void setCategoryCode(String categoryCode) {
        this.categoryCode = categoryCode;
    }

    public Integer getPosition() {
        return position;
    }

    public void setPosition(Integer position) {
        this.position = position;
    }

    public Boolean getTemplateCompleted() {
        return templateCompleted;
    }

    public void setTemplateCompleted(Boolean templateCompleted) {
        this.templateCompleted = templateCompleted;
    }

    public Long getOrganizationId() {
        return organizationId;
    }

    public void setOrganizationId(Long organizationId) {
        this.organizationId = organizationId;
    }
}
