package io.choerodon.agile.infra.dto;

import io.choerodon.mybatis.annotation.ModifyAudit;
import io.choerodon.mybatis.annotation.VersionAudit;
import io.choerodon.mybatis.domain.AuditDomain;
import org.hzero.starter.keyencrypt.core.Encrypt;

import javax.persistence.Column;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.Table;

/**
 * @author zhaotianxin
 * @date 2021-03-25 10:44
 */
@Table(name = "fd_status_template")
@ModifyAudit
@VersionAudit
public class StatusTemplateDTO extends AuditDomain {
    @Id
    @GeneratedValue
    @Encrypt
    private Long id;

    private Long organizationId;

    @Encrypt
    private Long statusId;

    @Column(name = "is_template_completed")
    private Boolean templateCompleted;

    public StatusTemplateDTO() {
    }

    public StatusTemplateDTO(Long organizationId, Long statusId) {
        this.organizationId = organizationId;
        this.statusId = statusId;
    }

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public Long getOrganizationId() {
        return organizationId;
    }

    public void setOrganizationId(Long organizationId) {
        this.organizationId = organizationId;
    }

    public Long getStatusId() {
        return statusId;
    }

    public void setStatusId(Long statusId) {
        this.statusId = statusId;
    }

    public Boolean getTemplateCompleted() {
        return templateCompleted;
    }

    public void setTemplateCompleted(Boolean templateCompleted) {
        this.templateCompleted = templateCompleted;
    }
}
