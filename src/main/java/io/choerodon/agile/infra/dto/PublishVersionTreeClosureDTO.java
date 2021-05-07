package io.choerodon.agile.infra.dto;

import io.choerodon.mybatis.annotation.ModifyAudit;
import io.choerodon.mybatis.annotation.VersionAudit;
import io.choerodon.mybatis.domain.AuditDomain;

import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.Table;
import java.util.Objects;

/**
 * @author superlee
 * @since 2021-03-10
 */
@VersionAudit
@ModifyAudit
@Table(name = "agile_publish_version_tree_closure")
public class PublishVersionTreeClosureDTO extends AuditDomain {

    @Id
    @GeneratedValue
    private Long id;

    private Long ancestorId;

    private Long descendantId;

    private Long descendantParent;

    private Long projectId;

    private Long organizationId;

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public Long getAncestorId() {
        return ancestorId;
    }

    public void setAncestorId(Long ancestorId) {
        this.ancestorId = ancestorId;
    }

    public Long getDescendantId() {
        return descendantId;
    }

    public void setDescendantId(Long descendantId) {
        this.descendantId = descendantId;
    }

    public Long getDescendantParent() {
        return descendantParent;
    }

    public void setDescendantParent(Long descendantParent) {
        this.descendantParent = descendantParent;
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

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof PublishVersionTreeClosureDTO)) return false;
        PublishVersionTreeClosureDTO that = (PublishVersionTreeClosureDTO) o;
        return Objects.equals(getAncestorId(), that.getAncestorId()) &&
                Objects.equals(getDescendantId(), that.getDescendantId()) &&
                Objects.equals(getDescendantParent(), that.getDescendantParent()) &&
                Objects.equals(getProjectId(), that.getProjectId()) &&
                Objects.equals(getOrganizationId(), that.getOrganizationId());
    }

    @Override
    public int hashCode() {
        return Objects.hash(getAncestorId(), getDescendantId(), getDescendantParent(), getProjectId(), getOrganizationId());
    }
}
