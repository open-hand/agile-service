package io.choerodon.agile.infra.dto;


import io.choerodon.mybatis.annotation.ModifyAudit;
import io.choerodon.mybatis.annotation.VersionAudit;
import io.choerodon.mybatis.domain.AuditDomain;
import org.hzero.starter.keyencrypt.core.Encrypt;

import javax.persistence.*;

/**
 * Created by HuangFuqiang@choerodon.io on 2019/2/25.
 * Email: fuqianghuang01@gmail.com
 */
@Table(name = "agile_file_operation_history")
@ModifyAudit
@VersionAudit
public class FileOperationHistoryDTO extends AuditDomain {

    public FileOperationHistoryDTO() {}

    public FileOperationHistoryDTO(Long projectId, Long userId, String action, Long successCount, Long failCount, String status) {
        this.projectId = projectId;
        this.userId = userId;
        this.action = action;
        this.successCount = successCount;
        this.failCount = failCount;
        this.status = status;
    }

    public FileOperationHistoryDTO(Long projectId, Long organizationId, Long userId, String action, Long successCount, Long failCount, String status) {
        this.organizationId = organizationId;
        this.projectId = projectId;
        this.userId = userId;
        this.action = action;
        this.successCount = successCount;
        this.failCount = failCount;
        this.status = status;
    }

    public FileOperationHistoryDTO(Long projectId, Long id, String action, String status, Long objectVersionNumber) {
        this.projectId = projectId;
        this.id = id;
        this.action = action;
        this.status = status;
        this.objectVersionNumber = objectVersionNumber;
    }

    @Id
    @GeneratedValue
    @Encrypt
    private Long id;

    private Long projectId;

    private Long organizationId;

    private Long userId;

    private String action;

    private Long successCount;

    private Long failCount;

    private String status;

    private String fileUrl;

    private Long objectVersionNumber;

    @Transient
    private Double process;
    @Transient
    private String msg;

    public String getMsg() {
        return msg;
    }

    public void setMsg(String msg) {
        this.msg = msg;
    }

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public void setProjectId(Long projectId) {
        this.projectId = projectId;
    }

    public Long getProjectId() {
        return projectId;
    }

    public String getAction() {
        return action;
    }

    public void setAction(String action) {
        this.action = action;
    }

    public Long getSuccessCount() {
        return successCount;
    }

    public void setSuccessCount(Long successCount) {
        this.successCount = successCount;
    }

    public Long getFailCount() {
        return failCount;
    }

    public void setFailCount(Long failCount) {
        this.failCount = failCount;
    }

    public void setStatus(String status) {
        this.status = status;
    }

    public String getStatus() {
        return status;
    }

    public void setUserId(Long userId) {
        this.userId = userId;
    }

    public Long getUserId() {
        return userId;
    }

    public void setFileUrl(String fileUrl) {
        this.fileUrl = fileUrl;
    }

    public String getFileUrl() {
        return fileUrl;
    }

    @Override
    public void setObjectVersionNumber(Long objectVersionNumber) {
        this.objectVersionNumber = objectVersionNumber;
    }

    @Override
    public Long getObjectVersionNumber() {
        return objectVersionNumber;
    }

    public void setProcess(Double process) {
        this.process = process;
    }

    public Double getProcess() {
        return process;
    }

    public Long getOrganizationId() {
        return organizationId;
    }

    public void setOrganizationId(Long organizationId) {
        this.organizationId = organizationId;
    }

    @Override
    public String toString() {
        return "FileOperationHistoryDTO{" +
                "id=" + id +
                ", projectId=" + projectId +
                ", organizationId=" + organizationId +
                ", userId=" + userId +
                ", action='" + action + '\'' +
                ", successCount=" + successCount +
                ", failCount=" + failCount +
                ", status='" + status + '\'' +
                ", fileUrl='" + fileUrl + '\'' +
                ", objectVersionNumber=" + objectVersionNumber +
                ", process=" + process +
                ", msg='" + msg + '\'' +
                ", creationDate='" + getCreationDate() + '\'' +
                ", lastUpdateDate='" + getLastUpdateDate() + '\'' +
                '}';
    }
}
