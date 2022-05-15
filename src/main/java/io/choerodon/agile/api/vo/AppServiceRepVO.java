package io.choerodon.agile.api.vo;


import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

import java.util.Date;

/**
 * @author younger
 * @date 2018/3/30
 */
public class AppServiceRepVO {
    @ApiModelProperty("应用服务id")
    @Encrypt
    private Long id;

    @ApiModelProperty("应用服务名称")
    private String name;

    @ApiModelProperty("应用服务code")
    private String code;

    @ApiModelProperty("应用服务所属项目id")
    private Long projectId;

    @ApiModelProperty("应用服务对应gitlab项目的id")
    private Long gitlabProjectId;

    @ApiModelProperty("应用服务对应的gitlab仓库地址")
    private String repoUrl;

    @ApiModelProperty("应用服务对应的gitlab的仓库的ssh协议克隆地址")
    private String sshRepositoryUrl;

    @ApiModelProperty("应用服务是否同步完成，false表示正在处理中")
    private Boolean synchro;

    @ApiModelProperty("应用服务是否启用")
    private Boolean isActive;
    @ApiModelProperty(value = "发布等级")
    private String publishLevel;
    @ApiModelProperty(value = "贡献者")
    private String contributor;

    @ApiModelProperty("应用服务描述")
    private String description;

    @ApiModelProperty("sonarqube地址")
    private String sonarUrl;

    @ApiModelProperty("应用服务是否失败，如果已同步且这个值为true说明应用服务创建失败")
    private Boolean fail;

    @ApiModelProperty("应用服务的类型")
    private String type;

    @ApiModelProperty("应用服务数据库纪录的版本号")
    private Long objectVersionNumber;

    @ApiModelProperty("应用服务图标url")
    private String imgUrl;

    @ApiModelProperty("应用创建时间")
    private Date creationDate;

    @ApiModelProperty("应用服务最近更新时间")
    private Date lastUpdateDate;

    @ApiModelProperty("创建者用户名")
    private String createUserName;

    @ApiModelProperty("创建者登录名")
    private String createLoginName;

    @ApiModelProperty("最近更新者用户名")
    private String updateUserName;

    @ApiModelProperty("最近更新者登录名")
    private String updateLoginName;

    @ApiModelProperty("是否是空仓库(是否没有分支)")
    private Boolean emptyRepository;

    @ApiModelProperty("应用服务类型")
    private String serviceType;

    @ApiModelProperty("来源项目名")
    private String shareProjectName;

    @ApiModelProperty("事务实例id")
    @Encrypt
    private Long sagaInstanceId;
    @ApiModelProperty(value = "artifactId")
    private String artifactId;
    @ApiModelProperty(value = "groupId")
    private String groupId;

    public String getArtifactId() {
        return artifactId;
    }

    public void setArtifactId(String artifactId) {
        this.artifactId = artifactId;
    }

    public String getGroupId() {
        return groupId;
    }

    public void setGroupId(String groupId) {
        this.groupId = groupId;
    }

    public Long getSagaInstanceId() {
        return sagaInstanceId;
    }

    public void setSagaInstanceId(Long sagaInstanceId) {
        this.sagaInstanceId = sagaInstanceId;
    }

    public String getShareProjectName() {
        return shareProjectName;
    }

    public void setShareProjectName(String shareProjectName) {
        this.shareProjectName = shareProjectName;
    }

    public Date getCreationDate() {
        return creationDate;
    }

    public void setCreationDate(Date creationDate) {
        this.creationDate = creationDate;
    }

    public String getImgUrl() {
        return imgUrl;
    }

    public void setImgUrl(String imgUrl) {
        this.imgUrl = imgUrl;
    }

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

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

    public Long getProjectId() {
        return projectId;
    }

    public void setProjectId(Long projectId) {
        this.projectId = projectId;
    }

    public String getRepoUrl() {
        return repoUrl;
    }

    public void setRepoUrl(String repoUrl) {
        this.repoUrl = repoUrl;
    }

    public Boolean getSynchro() {
        return synchro;
    }

    public void setSynchro(Boolean synchro) {
        this.synchro = synchro;
    }

    public Boolean getActive() {
        return isActive;
    }

    public void setActive(Boolean active) {
        isActive = active;
    }

    public String getPublishLevel() {
        return publishLevel;
    }

    public void setPublishLevel(String publishLevel) {
        this.publishLevel = publishLevel;
    }

    public String getContributor() {
        return contributor;
    }

    public void setContributor(String contributor) {
        this.contributor = contributor;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public String getSonarUrl() {
        return sonarUrl;
    }

    public void setSonarUrl(String sonarUrl) {
        this.sonarUrl = sonarUrl;
    }

    public Boolean getFail() {
        return fail;
    }

    public void setFail(Boolean fail) {
        this.fail = fail;
    }

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    public Long getObjectVersionNumber() {
        return objectVersionNumber;
    }

    public void setObjectVersionNumber(Long objectVersionNumber) {
        this.objectVersionNumber = objectVersionNumber;
    }

    public Long getGitlabProjectId() {
        return gitlabProjectId;
    }

    public void setGitlabProjectId(Long gitlabProjectId) {
        this.gitlabProjectId = gitlabProjectId;
    }

    public Date getLastUpdateDate() {
        return lastUpdateDate;
    }

    public void setLastUpdateDate(Date lastUpdateDate) {
        this.lastUpdateDate = lastUpdateDate;
    }

    public String getCreateUserName() {
        return createUserName;
    }

    public void setCreateUserName(String createUserName) {
        this.createUserName = createUserName;
    }

    public String getCreateLoginName() {
        return createLoginName;
    }

    public void setCreateLoginName(String createLoginName) {
        this.createLoginName = createLoginName;
    }

    public String getUpdateUserName() {
        return updateUserName;
    }

    public void setUpdateUserName(String updateUserName) {
        this.updateUserName = updateUserName;
    }

    public String getUpdateLoginName() {
        return updateLoginName;
    }

    public void setUpdateLoginName(String updateLoginName) {
        this.updateLoginName = updateLoginName;
    }

    public Boolean getEmptyRepository() {
        return emptyRepository;
    }

    public void setEmptyRepository(Boolean emptyRepository) {
        this.emptyRepository = emptyRepository;
    }

    public String getSshRepositoryUrl() {
        return sshRepositoryUrl;
    }

    public void setSshRepositoryUrl(String sshRepositoryUrl) {
        this.sshRepositoryUrl = sshRepositoryUrl;
    }

    public String getServiceType() {
        return serviceType;
    }

    public void setServiceType(String serviceType) {
        this.serviceType = serviceType;
    }

}

