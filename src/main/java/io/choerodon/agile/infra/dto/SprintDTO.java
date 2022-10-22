package io.choerodon.agile.infra.dto;

import java.util.Date;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.Table;
import javax.validation.constraints.NotNull;

import io.swagger.annotations.ApiModelProperty;
import org.hibernate.validator.constraints.Length;

import io.choerodon.mybatis.annotation.ModifyAudit;
import io.choerodon.mybatis.annotation.VersionAudit;
import io.choerodon.mybatis.domain.AuditDomain;

import org.hzero.starter.keyencrypt.core.Encrypt;

/**
 * Created by jian_zhang02@163.com on 2018/5/14.
 */

@Table(name = "agile_sprint")
@ModifyAudit
@VersionAudit
public class SprintDTO extends AuditDomain {

    /**
     * 冲刺目标最大长度
     */
    public static final int MAX_SPRINT_GAOL_LENGTH = 255;

    @Id
    @GeneratedValue
    @Encrypt
    private Long sprintId;
    private String sprintName;
    @Length(max = SprintDTO.MAX_SPRINT_GAOL_LENGTH)
    private String sprintGoal;
    private Date startDate;
    private Date endDate;
    private Date actualEndDate;

    /**
     * sprint_planning: 规划中
     * closed: 已完成
     * started: 活跃
     */
    @ApiModelProperty("冲刺状态")
    private String statusCode;
    @NotNull
    private Long projectId;

    public Long getSprintId() {
        return sprintId;
    }

    public void setSprintId(Long sprintId) {
        this.sprintId = sprintId;
    }

    public String getSprintName() {
        return sprintName;
    }

    public void setSprintName(String sprintName) {
        this.sprintName = sprintName;
    }

    public String getSprintGoal() {
        return sprintGoal;
    }

    public void setSprintGoal(String sprintGoal) {
        this.sprintGoal = sprintGoal;
    }

    public Date getStartDate() {
        return startDate;
    }

    public void setStartDate(Date startDate) {
        this.startDate = startDate;
    }

    public Date getEndDate() {
        return endDate;
    }

    public void setEndDate(Date endDate) {
        this.endDate = endDate;
    }

    public Date getActualEndDate() {
        return actualEndDate;
    }

    public void setActualEndDate(Date actualEndDate) {
        this.actualEndDate = actualEndDate;
    }

    public String getStatusCode() {
        return statusCode;
    }

    public void setStatusCode(String statusCode) {
        this.statusCode = statusCode;
    }

    public Long getProjectId() {
        return projectId;
    }

    public void setProjectId(Long projectId) {
        this.projectId = projectId;
    }

}
