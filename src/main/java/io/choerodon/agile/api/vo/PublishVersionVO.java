package io.choerodon.agile.api.vo;

import io.choerodon.agile.infra.dto.UserMessageDTO;
import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

import javax.validation.constraints.NotNull;
import java.util.Date;
import java.util.List;

/**
 * @author chihao.ran@hand-china.com
 * 2021/03/11 19:25
 */
public class PublishVersionVO {

    @ApiModelProperty(value = "发布版本id")
    @Encrypt
    private Long id;
    @ApiModelProperty(value = "发布版本组id")
    private String groupId;
    @ApiModelProperty(value = "发布版本制品id")
    private String artifactId;
    @ApiModelProperty(value = "发布版本")
    private String version;
    @ApiModelProperty(value = "版本别名")
    private String versionAlias;
    @ApiModelProperty(value = "服务编码")
    private String serviceCode;
    @ApiModelProperty(value = "项目id")
    @NotNull(message = "error.projectId.null")
    private Long projectId;
    @ApiModelProperty(value = "组织id")
    @NotNull(message = "error.organizationId.null")
    private Long organizationId;
    @ApiModelProperty(value = "版本号")
    private Long objectVersionNumber;
    @ApiModelProperty(value = "是否为应用服务")
    @NotNull(message = "error.appService.null")
    private Boolean appService;
    @ApiModelProperty(value = "子版本")
    private List<PublishVersionVO> children;
    @ApiModelProperty(value = "实际发布时间")
    private Date actualPublishDate;
    @ApiModelProperty(value = "描述")
    private String description;

    private String content;

    private Date creationDate;

    private Date lastUpdateDate;

    private String appServiceName;

    private String statusCode;

    private UserMessageDTO creator;

    private UserMessageDTO updater;

    private String tagName;

    public String getTagName() {
        return tagName;
    }

    public void setTagName(String tagName) {
        this.tagName = tagName;
    }

    public UserMessageDTO getCreator() {
        return creator;
    }

    public void setCreator(UserMessageDTO creator) {
        this.creator = creator;
    }

    public UserMessageDTO getUpdater() {
        return updater;
    }

    public void setUpdater(UserMessageDTO updater) {
        this.updater = updater;
    }

    public String getStatusCode() {
        return statusCode;
    }

    public void setStatusCode(String statusCode) {
        this.statusCode = statusCode;
    }

    public String getAppServiceName() {
        return appServiceName;
    }

    public void setAppServiceName(String appServiceName) {
        this.appServiceName = appServiceName;
    }

    public Date getCreationDate() {
        return creationDate;
    }

    public void setCreationDate(Date creationDate) {
        this.creationDate = creationDate;
    }

    public Date getLastUpdateDate() {
        return lastUpdateDate;
    }

    public void setLastUpdateDate(Date lastUpdateDate) {
        this.lastUpdateDate = lastUpdateDate;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public String getContent() {
        return content;
    }

    public void setContent(String content) {
        this.content = content;
    }

    public Date getActualPublishDate() {
        return actualPublishDate;
    }

    public void setActualPublishDate(Date actualPublishDate) {
        this.actualPublishDate = actualPublishDate;
    }

    public List<PublishVersionVO> getChildren() {
        return children;
    }

    public void setChildren(List<PublishVersionVO> children) {
        this.children = children;
    }

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

    public Long getObjectVersionNumber() {
        return objectVersionNumber;
    }

    public void setObjectVersionNumber(Long objectVersionNumber) {
        this.objectVersionNumber = objectVersionNumber;
    }
}
