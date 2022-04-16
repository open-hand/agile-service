package io.choerodon.agile.api.vo;

import org.hzero.starter.keyencrypt.core.Encrypt;

public class BacklogInfoVO {
    @Encrypt
    private Long backlogId;

    private Long organizationId;

    private Long projectId;

    private Long statusId;

    public BacklogInfoVO() {
    }

    public BacklogInfoVO(Long backlogId, Long organizationId, Long projectId) {
        this.backlogId = backlogId;
        this.organizationId = organizationId;
        this.projectId = projectId;
    }

    public BacklogInfoVO(Long backlogId, Long organizationId, Long projectId, Long statusId) {
        this.backlogId = backlogId;
        this.organizationId = organizationId;
        this.projectId = projectId;
        this.statusId = statusId;
    }

    public Long getStatusId() {
        return statusId;
    }

    public void setStatusId(Long statusId) {
        this.statusId = statusId;
    }

    public Long getBacklogId() {
        return backlogId;
    }

    public void setBacklogId(Long backlogId) {
        this.backlogId = backlogId;
    }

    public Long getOrganizationId() {
        return organizationId;
    }

    public void setOrganizationId(Long organizationId) {
        this.organizationId = organizationId;
    }

    public Long getProjectId() {
        return projectId;
    }

    public void setProjectId(Long projectId) {
        this.projectId = projectId;
    }
}
