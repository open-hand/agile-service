package io.choerodon.agile.infra.dto;

import io.choerodon.mybatis.annotation.ModifyAudit;
import io.choerodon.mybatis.annotation.VersionAudit;
import io.choerodon.mybatis.domain.AuditDomain;
import io.swagger.annotations.ApiModelProperty;

import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.Table;
import javax.persistence.Transient;

/**
 * @author superlee
 * @since 2021-07-20
 */
@VersionAudit
@ModifyAudit
@Table(name = "fd_field_permission")
public class FieldPermissionDTO extends AuditDomain {

    @Id
    @GeneratedValue
    private Long id;

    private Long fieldId;

    private Long issueTypeId;

    private Long roleMemberId;

    private String type;

    private String permission;

    private Long projectId;

    private Long organizationId;

    @Transient
    @ApiModelProperty(value = "字段编码")
    private String fieldCode;

    public FieldPermissionDTO() {}

    public FieldPermissionDTO(Long id,
                              Long fieldId,
                              Long issueTypeId,
                              Long roleMemberId,
                              String type,
                              String permission,
                              Long projectId,
                              Long organizationId) {
        this.id = id;
        this.fieldId = fieldId;
        this.issueTypeId = issueTypeId;
        this.roleMemberId = roleMemberId;
        this.type = type;
        this.permission = permission;
        this.projectId = projectId;
        this.organizationId = organizationId;
    }

    public Long getIssueTypeId() {
        return issueTypeId;
    }

    public void setIssueTypeId(Long issueTypeId) {
        this.issueTypeId = issueTypeId;
    }

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public Long getFieldId() {
        return fieldId;
    }

    public void setFieldId(Long fieldId) {
        this.fieldId = fieldId;
    }

    public Long getRoleMemberId() {
        return roleMemberId;
    }

    public void setRoleMemberId(Long roleMemberId) {
        this.roleMemberId = roleMemberId;
    }

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    public String getPermission() {
        return permission;
    }

    public void setPermission(String permission) {
        this.permission = permission;
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

    public String getFieldCode() {
        return fieldCode;
    }

    public void setFieldCode(String fieldCode) {
        this.fieldCode = fieldCode;
    }
}
