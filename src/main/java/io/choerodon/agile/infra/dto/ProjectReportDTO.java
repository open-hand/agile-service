package io.choerodon.agile.infra.dto;

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

    @Id
    @GeneratedValue
    @ApiModelProperty("主键id")
    private Long id;
    @ApiModelProperty("项目id")
    private Long projectId;
    @ApiModelProperty("标题")
    private String title;
    @ApiModelProperty("状态")
    private String status;
    @ApiModelProperty("定时任务id")
    private Long jobId;
    @ApiModelProperty("收件人Id")
    private Long receiverId;
    @ApiModelProperty("报表数据")
    private String report_data;





}
