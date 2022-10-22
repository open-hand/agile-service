package io.choerodon.agile.api.vo;

import java.util.Date;
import javax.validation.constraints.NotBlank;

import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.annotations.ApiModelProperty;
import org.hibernate.validator.constraints.Length;

import io.choerodon.agile.infra.dto.SprintDTO;

/**
 * Created by HuangFuqiang@choerodon.io on 2019/9/27.
 * Email: fuqianghuang01@gmail.com
 */
@JsonInclude(JsonInclude.Include.NON_NULL)
public class SprintCreateVO {
    @NotBlank
    @ApiModelProperty(value = "冲刺名称")
    private String sprintName;
    @Length(max = SprintDTO.MAX_SPRINT_GAOL_LENGTH)
    @ApiModelProperty(value = "冲刺目标")
    private String sprintGoal;
    @ApiModelProperty(value = "开始时间")
    private Date startDate;
    @ApiModelProperty(value = "结束时间")
    private Date endDate;

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
}
