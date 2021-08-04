package io.choerodon.agile.infra.dto;

import io.choerodon.mybatis.annotation.ModifyAudit;
import io.choerodon.mybatis.annotation.VersionAudit;
import io.choerodon.mybatis.domain.AuditDomain;
import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.Table;

/**
 * @author zhaotianxin
 * @date 2021-08-03 9:34
 */
@Table(name = "fd_status_linkage_execution_log")
@ModifyAudit
@VersionAudit
public class StatusLinkageExecutionLogDTO extends AuditDomain {

    @Id
    @GeneratedValue
    @Encrypt
    private Long id;

    @ApiModelProperty(value = "执行状态(LOOP/SUCCESS)")
    private String statusCode;

    @ApiModelProperty(value = "联动信息")
    private String content;

    @Encrypt
    @ApiModelProperty(value = "上一个issue")
    private Long preIssueId;

    @Encrypt
    @ApiModelProperty(value = "当前的issue")
    private Long curIssueId;

    private Long projectId;

    private Long organizationId;

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public String getStatusCode() {
        return statusCode;
    }

    public void setStatusCode(String statusCode) {
        this.statusCode = statusCode;
    }

    public String getContent() {
        return content;
    }

    public void setContent(String content) {
        this.content = content;
    }

    public Long getPreIssueId() {
        return preIssueId;
    }

    public void setPreIssueId(Long preIssueId) {
        this.preIssueId = preIssueId;
    }

    public Long getCurIssueId() {
        return curIssueId;
    }

    public void setCurIssueId(Long curIssueId) {
        this.curIssueId = curIssueId;
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
