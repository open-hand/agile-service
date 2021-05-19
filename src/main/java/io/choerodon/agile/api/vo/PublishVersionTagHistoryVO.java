package io.choerodon.agile.api.vo;

import org.hzero.starter.keyencrypt.core.Encrypt;

import io.choerodon.mybatis.domain.AuditDomain;

/**
 * @author chihao.ran@hand-china.com
 * 2021/05/19 10:56
 */
public class PublishVersionTagHistoryVO extends AuditDomain {

    @Encrypt
    private Long id;

    private Long projectId;

    private Long organizationId;

    @Encrypt
    private Long publishVersionId;

    private String action;

    private String status;

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

    public Long getPublishVersionId() {
        return publishVersionId;
    }

    public void setPublishVersionId(Long publishVersionId) {
        this.publishVersionId = publishVersionId;
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
}
