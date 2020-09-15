package io.choerodon.agile.infra.dto;

import javax.persistence.GeneratedValue;
import javax.persistence.Id;

import io.choerodon.mybatis.domain.AuditDomain;
import io.swagger.annotations.ApiModelProperty;

/**
 * @author jiaxu.cui@hand-china.com 2020/9/15 下午2:31
 */
public class ProjectReportCcLineDTO extends AuditDomain {

    @Id
    @GeneratedValue
    @ApiModelProperty("主键id")
    private Long id;
    @ApiModelProperty("项目id")
    private Long projectId;
    @ApiModelProperty("项目报告id")
    private Long projectReportId;
    @ApiModelProperty("抄送人Id")
    private Long ccId;
}
