package io.choerodon.agile.infra.dto;

import io.choerodon.mybatis.annotation.ModifyAudit;
import io.choerodon.mybatis.annotation.VersionAudit;
import io.choerodon.mybatis.domain.AuditDomain;
import org.hzero.starter.keyencrypt.core.Encrypt;

import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.Table;

/**
 * @author zhaotianxin
 * @date 2020-08-17 19:10
 */
@Table(name = "fd_status_linkage")
@ModifyAudit
@VersionAudit
public class StatusLinkageDTO extends AuditDomain {

    @Id
    @GeneratedValue
    @Encrypt
    private Long id;

    @Encrypt
    private Long issueTypeId;
    @Encrypt
    private Long statusId;

    private Long projectId;

    private String  parentIssueTypeCode;

    @Encrypt
    private Long parentIssueStatusSetting;
    @Encrypt
    private Long parentIssueTypeId;

    private Long organizationId;

    private String type;

    public Long getParentIssueTypeId() {
        return parentIssueTypeId;
    }

    public void setParentIssueTypeId(Long parentIssueTypeId) {
        this.parentIssueTypeId = parentIssueTypeId;
    }

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public Long getIssueTypeId() {
        return issueTypeId;
    }

    public void setIssueTypeId(Long issueTypeId) {
        this.issueTypeId = issueTypeId;
    }

    public Long getStatusId() {
        return statusId;
    }

    public void setStatusId(Long statusId) {
        this.statusId = statusId;
    }

    public Long getProjectId() {
        return projectId;
    }

    public void setProjectId(Long projectId) {
        this.projectId = projectId;
    }

    public String getParentIssueTypeCode() {
        return parentIssueTypeCode;
    }

    public void setParentIssueTypeCode(String parentIssueTypeCode) {
        this.parentIssueTypeCode = parentIssueTypeCode;
    }

    public Long getParentIssueStatusSetting() {
        return parentIssueStatusSetting;
    }

    public void setParentIssueStatusSetting(Long parentIssueStatusSetting) {
        this.parentIssueStatusSetting = parentIssueStatusSetting;
    }

    public Long getOrganizationId() {
        return organizationId;
    }

    public void setOrganizationId(Long organizationId) {
        this.organizationId = organizationId;
    }

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }
}
