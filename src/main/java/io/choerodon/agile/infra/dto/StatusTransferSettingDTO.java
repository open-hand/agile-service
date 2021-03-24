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
 * @date 2020-08-12 9:53
 */
@Table(name = "fd_status_transfer_setting")
@ModifyAudit
@VersionAudit
public class StatusTransferSettingDTO extends AuditDomain {
    @Id
    @GeneratedValue
    @Encrypt
    private Long id;
    @Encrypt
    private Long issueTypeId;
    @Encrypt
    private Long statusId;

    private Long projectId;

    private String userType;
    @Encrypt
    private Long userId;

    private Long organizationId;

    public StatusTransferSettingDTO() {
    }

    public StatusTransferSettingDTO(Long issueTypeId, Long statusId, Long projectId, String userType) {
        this.issueTypeId = issueTypeId;
        this.statusId = statusId;
        this.projectId = projectId;
        this.userType = userType;
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

    public String getUserType() {
        return userType;
    }

    public void setUserType(String userType) {
        this.userType = userType;
    }

    public Long getUserId() {
        return userId;
    }

    public void setUserId(Long userId) {
        this.userId = userId;
    }

    public Long getOrganizationId() {
        return organizationId;
    }

    public void setOrganizationId(Long organizationId) {
        this.organizationId = organizationId;
    }
}
