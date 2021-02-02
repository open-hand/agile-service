package io.choerodon.agile.infra.dto;

import io.choerodon.mybatis.annotation.ModifyAudit;
import io.choerodon.mybatis.annotation.VersionAudit;
import io.choerodon.mybatis.domain.AuditDomain;

import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.Table;

/**
 * @author huaxin.deng@hand-china.com 2021-02-02 13:58:04
 */
@Table(name = "fd_personal_template")
@ModifyAudit
@VersionAudit
public class PersonalTemplateDTO extends AuditDomain {
    @Id
    @GeneratedValue
    private Long id;

    private String action;

    private String type;

    private Long projectId;

    private Long organizationId;

    private Long userId;

    private String name;

    private String templateJson;

    public PersonalTemplateDTO() {
    }

    public PersonalTemplateDTO(Long id, String action, String type, Long projectId, Long userId) {
        this.id = id;
        this.action = action;
        this.type = type;
        this.projectId = projectId;
        this.userId = userId;
    }

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public String getAction() {
        return action;
    }

    public void setAction(String action) {
        this.action = action;
    }

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
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

    public Long getUserId() {
        return userId;
    }

    public void setUserId(Long userId) {
        this.userId = userId;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getTemplateJson() {
        return templateJson;
    }

    public void setTemplateJson(String templateJson) {
        this.templateJson = templateJson;
    }
}
