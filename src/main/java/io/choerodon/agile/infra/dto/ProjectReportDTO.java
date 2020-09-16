package io.choerodon.agile.infra.dto;

import java.util.Date;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.Table;

import io.choerodon.mybatis.annotation.ModifyAudit;
import io.choerodon.mybatis.annotation.VersionAudit;
import io.choerodon.mybatis.domain.AuditDomain;
import io.swagger.annotations.ApiModelProperty;

/**
 * @author jiaxu.cui@hand-china.com 2020/9/15 上午11:00
 */
@Table(name = "agile_project_report")
@ModifyAudit
@VersionAudit
public class ProjectReportDTO extends AuditDomain {

    public ProjectReportDTO(Long id, Long projectId) {
        this.id = id;
        this.projectId = projectId;
    }

    public ProjectReportDTO() {
    }

    @Id
    @GeneratedValue
    @ApiModelProperty("主键id")
    private Long id;
    @ApiModelProperty("项目id")
    private Long projectId;
    @ApiModelProperty("报表标题")
    private String title;
    @ApiModelProperty("报表描述")
    private String description;
    @ApiModelProperty("状态")
    private String status;
    @ApiModelProperty("报表数据")
    private String reportData;
    @ApiModelProperty("最近发送时间")
    private Date recentSendDate;

    public Date getRecentSendDate() {
        return recentSendDate;
    }

    public void setRecentSendDate(Date recentSendDate) {
        this.recentSendDate = recentSendDate;
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

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public void setProjectId(Long projectId) {
        this.projectId = projectId;
    }

    public String getTitle() {
        return title;
    }

    public void setTitle(String title) {
        this.title = title;
    }

    public String getStatus() {
        return status;
    }

    public void setStatus(String status) {
        this.status = status;
    }


    public String getReportData() {
        return reportData;
    }

    public void setReportData(String reportData) {
        this.reportData = reportData;
    }
}
