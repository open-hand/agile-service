package io.choerodon.agile.api.vo;

import io.swagger.annotations.ApiModelProperty;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;

/**
 * @author chihao.ran@hand-china.com
 * 2021/03/12 11:32
 */
public class AppVersionCreateVO {
    @NotEmpty(message = "error.field.artifactIdNotNull")
    @ApiModelProperty(value = "应用版本名称")
    private String artifactId;
    @NotEmpty(message = "error.field.versionNotNull")
    @ApiModelProperty(value = "应用版本")
    private String version;
    @ApiModelProperty(value = "版本别名")
    private String versionAlias;
    @ApiModelProperty(value = "服务编码")
    @NotEmpty(message = "error.field.serviceCodeNull")
    private String serviceCode;
    @ApiModelProperty(value = "是否为tag")
    private Boolean tag;
    @ApiModelProperty(value = "是否为应用服务")
    @NotNull(message = "error.field.appServiceNull")
    private Boolean appService;

    public Boolean getTag() {
        return tag;
    }

    public void setTag(Boolean tag) {
        this.tag = tag;
    }

    public Boolean getAppService() {
        return appService;
    }

    public void setAppService(Boolean appService) {
        this.appService = appService;
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
}
