package io.choerodon.agile.infra.dto;

import io.choerodon.agile.infra.utils.StringUtil;
import io.choerodon.mybatis.annotation.ModifyAudit;
import io.choerodon.mybatis.annotation.VersionAudit;
import io.choerodon.mybatis.domain.AuditDomain;

import javax.persistence.GeneratedValue;

import javax.persistence.Id;
import javax.persistence.Table;

/**
 * @author dinghuang123@gmail.com
 * @since 2018/7/6
 */
@Table(name = "agile_issue_sprint_rel")
@ModifyAudit
@VersionAudit
public class IssueSprintRelDTO extends AuditDomain {

    @Id
    @GeneratedValue
    private Long id;  //需要加主键id

    private Long issueId;

    private Long sprintId;

    private Long projectId;

    public void setId(Long id) {
        this.id = id;
    }

    public Long getId() {
        return id;
    }

    public Long getIssueId() {
        return issueId;
    }

    public void setIssueId(Long issueId) {
        this.issueId = issueId;
    }

    public Long getSprintId() {
        return sprintId;
    }

    public void setSprintId(Long sprintId) {
        this.sprintId = sprintId;
    }

    public Long getProjectId() {
        return projectId;
    }

    public void setProjectId(Long projectId) {
        this.projectId = projectId;
    }

    @Override
    public String toString() {
        return StringUtil.getToString(this);
    }
}
