package io.choerodon.agile.api.vo;

import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

/**
 * @author huaxin.deng@hand-china.com 2021-02-02 13:54:38
 */
public class PersonalTemplatelVO {

    @ApiModelProperty(value = "excel模板id")
    @Encrypt
    private Long id;

    @ApiModelProperty(value = "action")
    private String action;

    @ApiModelProperty(value = "模板文件类型")
    private String type;

    @ApiModelProperty(value = "项目id")
    private Long projectId;

    @ApiModelProperty(value = "组织id")
    private Long organizationId;

    @ApiModelProperty(value = "用户id")
    @Encrypt
    private Long userId;

    @ApiModelProperty(value = "模板名称")
    private String name;

    @ApiModelProperty(value = "模板json")
    private String templateJson;

    @ApiModelProperty(value = "版本号")
    private Long objectVersionNumber;

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

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

    public Long getObjectVersionNumber() {
        return objectVersionNumber;
    }

    public void setObjectVersionNumber(Long objectVersionNumber) {
        this.objectVersionNumber = objectVersionNumber;
    }

    @Override
    public String toString() {
        return "PersonalTemplatelVO{" +
                "id=" + id +
                ", action='" + action + '\'' +
                ", type='" + type + '\'' +
                ", projectId=" + projectId +
                ", organizationId=" + organizationId +
                ", userId=" + userId +
                ", name='" + name + '\'' +
                ", templateJson='" + templateJson + '\'' +
                ", objectVersionNumber=" + objectVersionNumber +
                '}';
    }
}
