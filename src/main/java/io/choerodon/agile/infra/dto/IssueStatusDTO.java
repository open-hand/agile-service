package io.choerodon.agile.infra.dto;

import io.choerodon.mybatis.annotation.ModifyAudit;
import io.choerodon.mybatis.annotation.VersionAudit;
import io.choerodon.mybatis.domain.AuditDomain;

import javax.persistence.*;

/**
 * Created by HuangFuqiang@choerodon.io on 2018/5/14.
 * Email: fuqianghuang01@gmail.com
 */
@Table(name = "agile_issue_status")
@ModifyAudit
@VersionAudit
public class IssueStatusDTO extends AuditDomain {
    public static final String FIELD_PROJECT_ID = "projectId";
    public static final String FILED_IS_COMPLETED = "completed";

    @Id
    @GeneratedValue
    private Long id;

    private Long projectId;

    private String name;

    @Column(name = "is_enable")
    private Boolean enable;

    private String categoryCode;

    @Column(name = "is_completed")
    private Boolean completed;

    private Long statusId;

    public void setId(Long id) {
        this.id = id;
    }

    public Long getId() {
        return id;
    }

    public IssueStatusDTO setProjectId(Long projectId) {
        this.projectId = projectId;
        return this;
    }

    public Long getProjectId() {
        return projectId;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public void setEnable(Boolean enable) {
        this.enable = enable;
    }

    public Boolean getEnable() {
        return enable;
    }

    public void setCategoryCode(String categoryCode) {
        this.categoryCode = categoryCode;
    }

    public String getCategoryCode() {
        return categoryCode;
    }

    public void setCompleted(Boolean completed) {
        this.completed = completed;
    }

    public Boolean getCompleted() {
        return completed;
    }

    public void setStatusId(Long statusId) {
        this.statusId = statusId;
    }

    public Long getStatusId() {
        return statusId;
    }
}
