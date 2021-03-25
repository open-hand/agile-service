package io.choerodon.agile.infra.dto;

import io.choerodon.mybatis.annotation.ModifyAudit;
import io.choerodon.mybatis.annotation.VersionAudit;
import io.choerodon.mybatis.domain.AuditDomain;

import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.Table;
import java.util.Date;

/**
 * @author superlee
 * @since 2021-03-09
 */
@Table(name = "agile_publish_version")
@ModifyAudit
@VersionAudit
public class PublishVersionDTO extends AuditDomain {

    @Id
    @GeneratedValue
    private Long id;

    private String groupId;

    private String artifactId;

    private String version;

    private String versionAlias;

    private String serviceCode;

    private Long projectId;

    private Long organizationId;

    private Boolean appService;

    private Date actualPublishDate;

    private Long tagId;

    private String description;

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public Long getTagId() {
        return tagId;
    }

    public void setTagId(Long tagId) {
        this.tagId = tagId;
    }

    public Date getActualPublishDate() {
        return actualPublishDate;
    }

    public void setActualPublishDate(Date actualPublishDate) {
        this.actualPublishDate = actualPublishDate;
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
}
