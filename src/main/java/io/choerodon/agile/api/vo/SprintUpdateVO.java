package io.choerodon.agile.api.vo;


import java.util.Date;
import java.util.List;
import javax.validation.constraints.NotNull;

import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.annotations.ApiModelProperty;
import org.hibernate.validator.constraints.Length;

import io.choerodon.agile.infra.dto.SprintDTO;
import io.choerodon.agile.infra.utils.StringUtil;

import org.hzero.starter.keyencrypt.core.Encrypt;

/**
 * Created by jian_zhang02@163.com on 2018/5/15.
 *
 * @author dinghuang123@gmail.com
 */
@JsonInclude(JsonInclude.Include.NON_NULL)
public class SprintUpdateVO {
    private static final String OBJECT_VERSION_NUMBER_NULL_ERROR = "error.objectVersionNumber.NotNull";

    @ApiModelProperty(value = "冲刺id")
    @Encrypt
    private Long sprintId;

    @ApiModelProperty(value = "冲刺名称")
    private String sprintName;

    @Length(max = SprintDTO.MAX_SPRINT_GAOL_LENGTH)
    @ApiModelProperty(value = "冲刺目标")
    private String sprintGoal;

    @ApiModelProperty(value = "冲刺开始时间 ")
    private Date startDate;

    @ApiModelProperty(value = "冲刺结束时间")
    private Date endDate;

    @ApiModelProperty(value = "项目id")
    private Long projectId;

    @ApiModelProperty(value = "冲刺下的日历变更")
    private List<WorkCalendarRefVO> workDates;

    @ApiModelProperty(value = "版本号")
    @NotNull(message = OBJECT_VERSION_NUMBER_NULL_ERROR)
    private Long objectVersionNumber;

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

    public Long getProjectId() {
        return projectId;
    }

    public void setProjectId(Long projectId) {
        this.projectId = projectId;
    }

    public Long getObjectVersionNumber() {
        return objectVersionNumber;
    }

    public void setObjectVersionNumber(Long objectVersionNumber) {
        this.objectVersionNumber = objectVersionNumber;
    }

    public List<WorkCalendarRefVO> getWorkDates() {
        return workDates;
    }

    public void setWorkDates(List<WorkCalendarRefVO> workDates) {
        this.workDates = workDates;
    }

    @Override
    public String toString() {
        return StringUtil.getToString(this);
    }

}
