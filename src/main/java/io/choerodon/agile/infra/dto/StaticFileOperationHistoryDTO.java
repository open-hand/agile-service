package io.choerodon.agile.infra.dto;

import org.hzero.starter.keyencrypt.core.Encrypt;

import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.Table;
import javax.persistence.Transient;

import io.choerodon.mybatis.annotation.ModifyAudit;
import io.choerodon.mybatis.annotation.VersionAudit;
import io.choerodon.mybatis.domain.AuditDomain;

/**
 * @author chihao.ran@hand-china.com
 * 2021/01/21 12:00
 */
@Table(name = "agile_static_file_operation_history")
@ModifyAudit
@VersionAudit
public class StaticFileOperationHistoryDTO extends AuditDomain {
    @Id
    @GeneratedValue
    @Encrypt
    private Long id;
    private Long projectId;
    private Long organizationId;
    @Encrypt
    private Long staticFileId;
    private String action;
    private String status;
    private String errorMessage;
    @Encrypt
    private Long userId;

    @Transient
    private Double process;
    @Transient
    private String fileName;

    public StaticFileOperationHistoryDTO() {
    }

    public StaticFileOperationHistoryDTO(Long projectId, Long organizationId, Long staticFileId, Long userId, String action, String status) {
        this.projectId = projectId;
        this.organizationId = organizationId;
        this.staticFileId = staticFileId;
        this.action = action;
        this.status = status;
        this.userId = userId;
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

    public Long getStaticFileId() {
        return staticFileId;
    }

    public void setStaticFileId(Long staticFileId) {
        this.staticFileId = staticFileId;
    }

    public String getAction() {
        return action;
    }

    public void setAction(String action) {
        this.action = action;
    }

    public String getStatus() {
        return status;
    }

    public void setStatus(String status) {
        this.status = status;
    }

    public String getErrorMessage() {
        return errorMessage;
    }

    public void setErrorMessage(String errorMessage) {
        this.errorMessage = errorMessage;
    }

    public Long getUserId() {
        return userId;
    }

    public void setUserId(Long userId) {
        this.userId = userId;
    }

    public Double getProcess() {
        return process;
    }

    public void setProcess(Double process) {
        this.process = process;
    }

    public String getFileName() {
        return fileName;
    }

    public void setFileName(String fileName) {
        this.fileName = fileName;
    }
}
