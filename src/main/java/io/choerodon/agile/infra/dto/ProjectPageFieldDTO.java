package io.choerodon.agile.infra.dto;

import io.choerodon.mybatis.annotation.ModifyAudit;
import io.choerodon.mybatis.annotation.VersionAudit;
import io.choerodon.mybatis.domain.AuditDomain;

import javax.persistence.GeneratedValue;

import javax.persistence.Id;
import javax.persistence.Table;

/**
 * @author shinan.chen
 * @since 2019/4/3
 */
@Table(name = "fd_project_page_field")
@ModifyAudit
@VersionAudit
public class ProjectPageFieldDTO extends AuditDomain {
    @Id
    @GeneratedValue
    private Long id;
    private Long projectId;
    private Long organizationId;

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
}
