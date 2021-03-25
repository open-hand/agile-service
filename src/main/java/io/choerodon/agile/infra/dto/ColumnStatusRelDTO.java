package io.choerodon.agile.infra.dto;

import io.choerodon.agile.infra.utils.StringUtil;
import io.choerodon.mybatis.annotation.ModifyAudit;
import io.choerodon.mybatis.annotation.VersionAudit;
import io.choerodon.mybatis.domain.AuditDomain;

import javax.persistence.*;

/**
 * Created by HuangFuqiang@choerodon.io on 2018/5/15.
 * Email: fuqianghuang01@gmail.com
 */
@Table(name = "agile_board_column_status_rel")
@ModifyAudit
@VersionAudit
public class ColumnStatusRelDTO extends AuditDomain {

    @Id
    @GeneratedValue
    private Long id;  //需要加主键id

    private Integer position;

    private Long statusId;

    private Long columnId;

    private Long projectId;

    @Transient
    private Long issueId;

    private Long organizationId;

    public void setId(Long id) {
        this.id = id;
    }

    public Long getId() {
        return id;
    }

    public Integer getPosition() {
        return position;
    }

    public void setPosition(Integer position) {
        this.position = position;
    }

    public void setStatusId(Long statusId) {
        this.statusId = statusId;
    }

    public Long getStatusId() {
        return statusId;
    }

    public Long getColumnId() {
        return columnId;
    }

    public void setColumnId(Long columnId) {
        this.columnId = columnId;
    }

    public void setProjectId(Long projectId) {
        this.projectId = projectId;
    }

    public Long getProjectId() {
        return projectId;
    }

    public Long getIssueId() {
        return issueId;
    }

    public void setIssueId(Long issueId) {
        this.issueId = issueId;
    }

    @Override
    public String toString() {
        return StringUtil.getToString(this);
    }

    public Long getOrganizationId() {
        return organizationId;
    }

    public void setOrganizationId(Long organizationId) {
        this.organizationId = organizationId;
    }
}
