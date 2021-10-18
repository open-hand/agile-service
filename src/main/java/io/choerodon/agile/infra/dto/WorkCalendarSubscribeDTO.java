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
 * @author huaxin.deng@hand-china.com 2021-09-30 16:45:42
 */
@VersionAudit
@ModifyAudit
@Table(name = "agile_work_calendar_subscribe")
public class WorkCalendarSubscribeDTO extends AuditDomain {

    @Id
    @GeneratedValue
    @Encrypt
    private Long id;

    private Long organizationId;

    private Long userId;

    private String uuid;

    private String fileUrl;

    @Column(name = "is_changed")
    private Boolean changed;

    public WorkCalendarSubscribeDTO() {
    }

    public WorkCalendarSubscribeDTO(Long organizationId, Long userId, String fileUrl, String uuid) {
        this.organizationId = organizationId;
        this.userId = userId;
        this.fileUrl = fileUrl;
        this.uuid = uuid;
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

    public Long getUserId() {
        return userId;
    }

    public void setUserId(Long userId) {
        this.userId = userId;
    }

    public String getUuid() {
        return uuid;
    }

    public void setUuid(String uuid) {
        this.uuid = uuid;
    }

    public String getFileUrl() {
        return fileUrl;
    }

    public void setFileUrl(String fileUrl) {
        this.fileUrl = fileUrl;
    }

    public Boolean getChanged() {
        return changed;
    }

    public void setChanged(Boolean changed) {
        this.changed = changed;
    }
}
