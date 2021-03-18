package io.choerodon.agile.api.vo;

import io.swagger.annotations.ApiModelProperty;

import javax.validation.constraints.NotEmpty;

/**
 * @author chihao.ran@hand-china.com
 * 2021/03/12 11:32
 */
public class AppVersionUpdateVO {
    @ApiModelProperty(value = "应用版本")
    private String version;
    @ApiModelProperty(value = "版本别名")
    private String versionAlias;
    @ApiModelProperty(value = "版本号")
    private Long objectVersionNumber;

    @ApiModelProperty(value = "是否为tag")
    @NotEmpty(message = "error.field.tagNull")
    private Boolean tag;
    @ApiModelProperty(value = "是否为应用服务")
    @NotEmpty(message = "error.field.appServiceNull")
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

    public Long getObjectVersionNumber() {
        return objectVersionNumber;
    }

    public void setObjectVersionNumber(Long objectVersionNumber) {
        this.objectVersionNumber = objectVersionNumber;
    }
}
