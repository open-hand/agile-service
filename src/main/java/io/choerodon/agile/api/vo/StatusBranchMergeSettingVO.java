package io.choerodon.agile.api.vo;

import org.hzero.starter.keyencrypt.core.Encrypt;

/**
 * @author superlee
 * @since 2021-04-19
 */
public class StatusBranchMergeSettingVO {

    @Encrypt
    private Long id;
    @Encrypt
    private Long issueTypeId;
    @Encrypt
    private Long statusId;

    private Boolean autoTransform;

    private Long projectId;

    private Long organizationId;

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

    public Boolean getAutoTransform() {
        return autoTransform;
    }

    public void setAutoTransform(Boolean autoTransform) {
        this.autoTransform = autoTransform;
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
