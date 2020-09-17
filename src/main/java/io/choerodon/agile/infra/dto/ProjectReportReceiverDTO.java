package io.choerodon.agile.infra.dto;

import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.Table;

import io.choerodon.mybatis.annotation.ModifyAudit;
import io.choerodon.mybatis.annotation.VersionAudit;
import io.choerodon.mybatis.domain.AuditDomain;
import io.swagger.annotations.ApiModelProperty;

/**
 * @author jiaxu.cui@hand-china.com 2020/9/15 下午2:31
 */
@Table(name = "agile_project_report_receiver")
@ModifyAudit
@VersionAudit
public class ProjectReportReceiverDTO extends AuditDomain {
    
    public static final String TYPE_RECEIVER = "RECEIVER";
    public static final String TYPE_CC = "CC";

    public ProjectReportReceiverDTO(Long projectReportId, Long projectId) {
        this.projectReportId = projectReportId;
        this.projectId = projectId;
    }

    public ProjectReportReceiverDTO() {
    }

    @Id
    @GeneratedValue
    @ApiModelProperty("主键id")
    private Long id;
    @ApiModelProperty("项目id")
    private Long projectId;
    @ApiModelProperty("项目报告id")
    private Long projectReportId;
    @ApiModelProperty("接收人类型: RECEIVER|CC")
    private String type;
    @ApiModelProperty("收件人Id")
    private Long receiverId;

    public Long getReceiverId() {
        return receiverId;
    }

    public void setReceiverId(Long receiverId) {
        this.receiverId = receiverId;
    }

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

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

    public Long getProjectReportId() {
        return projectReportId;
    }

    public void setProjectReportId(Long projectReportId) {
        this.projectReportId = projectReportId;
    }
}
