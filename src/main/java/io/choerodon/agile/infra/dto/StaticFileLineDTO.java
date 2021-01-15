package io.choerodon.agile.infra.dto;

import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.Table;

import io.choerodon.mybatis.annotation.ModifyAudit;
import io.choerodon.mybatis.annotation.VersionAudit;
import io.choerodon.mybatis.domain.AuditDomain;

/**
 * @author chihao.ran@hand-china.com
 * 2021/01/13 16:07
 */
@Table(name = "agile_static_file_line")
@ModifyAudit
@VersionAudit
public class StaticFileLineDTO extends AuditDomain {
    @Id
    @GeneratedValue
    private Long id;
    private Long projectId;
    private Long organizationId;
    private Long headerId;
    private String url;
    private String relativePath;
    private String fileType;

    public StaticFileLineDTO(){
    }

    public StaticFileLineDTO(Long projectId, Long organizationId, Long headerId, String url, String relativePath) {
        this.projectId = projectId;
        this.organizationId = organizationId;
        this.headerId = headerId;
        this.url = url;
        this.relativePath = relativePath;
    }

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
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

    public Long getHeaderId() {
        return headerId;
    }

    public void setHeaderId(Long headerId) {
        this.headerId = headerId;
    }

    public String getUrl() {
        return url;
    }

    public void setUrl(String url) {
        this.url = url;
    }

    public String getRelativePath() {
        return relativePath;
    }

    public void setRelativePath(String relativePath) {
        this.relativePath = relativePath;
    }

    public String getFileType() {
        return fileType;
    }

    public void setFileType(String fileType) {
        this.fileType = fileType;
    }
}
