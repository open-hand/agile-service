package io.choerodon.agile.infra.dto;

import io.choerodon.mybatis.annotation.ModifyAudit;
import io.choerodon.mybatis.annotation.VersionAudit;
import io.choerodon.mybatis.domain.AuditDomain;
import org.hzero.starter.keyencrypt.core.Encrypt;

import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.Table;

/**
 * @author superlee
 * @since 2021-04-18
 */
@Table(name = "agile_tag_compare_history")
@ModifyAudit
@VersionAudit
public class TagCompareHistoryDTO extends AuditDomain {

    @Id
    @GeneratedValue
    @Encrypt
    private Long id;

    @Encrypt
    private Long publishVersionId;

    @Encrypt
    private Long publishVersionTagHistoryId;

    private String appServiceCode;

    private String source;

    private String target;

    private Long projectId;

    private Long organizationId;

    public Long getPublishVersionTagHistoryId() {
        return publishVersionTagHistoryId;
    }

    public void setPublishVersionTagHistoryId(Long publishVersionTagHistoryId) {
        this.publishVersionTagHistoryId = publishVersionTagHistoryId;
    }

    public Long getPublishVersionId() {
        return publishVersionId;
    }

    public void setPublishVersionId(Long publishVersionId) {
        this.publishVersionId = publishVersionId;
    }

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public String getAppServiceCode() {
        return appServiceCode;
    }

    public void setAppServiceCode(String appServiceCode) {
        this.appServiceCode = appServiceCode;
    }

    public String getSource() {
        return source;
    }

    public void setSource(String source) {
        this.source = source;
    }

    public String getTarget() {
        return target;
    }

    public void setTarget(String target) {
        this.target = target;
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
