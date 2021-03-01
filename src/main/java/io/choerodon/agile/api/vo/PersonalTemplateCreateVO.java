package io.choerodon.agile.api.vo;

import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

import javax.validation.constraints.NotNull;

/**
 * @author huaxin.deng@hand-china.com 2021-02-02 13:54:38
 */
public class PersonalTemplateCreateVO {

    @ApiModelProperty(value = "action")
    @NotNull(message = "error.template.actionNotNull")
    private String action;

    @ApiModelProperty(value = "模板文件类型")
    @NotNull(message = "error.template.typeNotNull")
    private String type;

    @ApiModelProperty(value = "项目id")
    @NotNull(message = "error.template.projectIdNotNull")
    private Long projectId;

    @ApiModelProperty(value = "组织id")
    @NotNull(message = "error.template.organizationIdNotNull")
    private Long organizationId;

    @ApiModelProperty(value = "用户id")
    @NotNull(message = "error.template.userIdNotNull")
    @Encrypt
    private Long userId;

    @ApiModelProperty(value = "模板名称")
    @NotNull(message = "error.template.nameNotNull")
    private String name;

    @ApiModelProperty(value = "模板json")
    @NotNull(message = "error.template.jsonNotNull")
    private String templateJson;

    public String getAction() {
        return action;
    }

    public void setAction(String action) {
        this.action = action;
    }

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    public Long getProjectId() {
        return projectId;
    }

    public void setProjectId(Long projectId) {
        this.projectId = projectId;
    }

    public Long getOrganizationId() {
        return organizationId;
    }

    public void setOrganizationId(Long organizationId) {
        this.organizationId = organizationId;
    }

    public Long getUserId() {
        return userId;
    }

    public void setUserId(Long userId) {
        this.userId = userId;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getTemplateJson() {
        return templateJson;
    }

    public void setTemplateJson(String templateJson) {
        this.templateJson = templateJson;
    }

    @Override
    public String toString() {
        return "PersonalTemplateCreateVO{" +
                "action='" + action + '\'' +
                ", type='" + type + '\'' +
                ", projectId=" + projectId +
                ", organizationId=" + organizationId +
                ", userId=" + userId +
                ", name='" + name + '\'' +
                ", templateJson='" + templateJson + '\'' +
                '}';
    }
}
