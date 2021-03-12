package io.choerodon.agile.api.vo;

import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

import javax.validation.constraints.NotEmpty;

/**
 * @author chihao.ran@hand-china.com
 * 2021/03/11 19:25
 */
public class AppVersionVO {

    @ApiModelProperty(value = "应用版本id")
    @Encrypt
    private Long id;
    @ApiModelProperty(value = "应用版本父组织标识符")
    @NotEmpty(message = "error.field.groupIdNotNull")
    private String groupId;
    @NotEmpty(message = "error.field.artifactIdNotNull")
    @ApiModelProperty(value = "应用版本名称")
    private String artifactId;
    @NotEmpty(message = "error.field.versionNotNull")
    @ApiModelProperty(value = "应用版本")
    private String version;
    @ApiModelProperty(value = "版本别名")
    private String versionAlias;
    @ApiModelProperty(value = "服务编码")
    private String serviceCode;
    @ApiModelProperty(value = "项目id")
    private Long projectId;
    @ApiModelProperty(value = "组织id")
    private Long organizationId;

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public String getGroupId() {
        return groupId;
    }

    public void setGroupId(String groupId) {
        this.groupId = groupId;
    }

    public String getArtifactId() {
        return artifactId;
    }

    public void setArtifactId(String artifactId) {
        this.artifactId = artifactId;
    }

    public String getVersion() {
        return version;
    }

    public void setVersion(String version) {
        this.version = version;
    }

    public String getVersionAlias() {
        return versionAlias;
    }

    public void setVersionAlias(String versionAlias) {
        this.versionAlias = versionAlias;
    }

    public String getServiceCode() {
        return serviceCode;
    }

    public void setServiceCode(String serviceCode) {
        this.serviceCode = serviceCode;
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
}
