package io.choerodon.agile.api.vo;

import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

import javax.validation.constraints.NotEmpty;

/**
 * @author chihao.ran@hand-china.com
 * 2021/03/11 19:25
 */
public class AppVersionSearchVO {

    @ApiModelProperty(value = "应用版本id")
    @Encrypt
    private Long id;
    @ApiModelProperty(value = "应用版本父组织标识符")
    private String groupId;
    @ApiModelProperty(value = "应用版本名称")
    private String artifactId;
    @ApiModelProperty(value = "应用版本")
    private String version;
    @ApiModelProperty(value = "版本别名")
    private String versionAlias;
    @ApiModelProperty(value = "服务编码")
    private String serviceCode;
    @ApiModelProperty(value = "版本或版本别名")
    private String content;
    @ApiModelProperty(value = "是否为应用服务")
    private Boolean appService;

    public Boolean getAppService() {
        return appService;
    }

    public void setAppService(Boolean appService) {
        this.appService = appService;
    }

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

    public String getContent() {
        return content;
    }

    public void setContent(String content) {
        this.content = content;
    }
}
