package io.choerodon.agile.api.vo;

import java.util.Date;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.annotations.ApiModelProperty;

/**
 * @author jiaxu.cui@hand-china.com 2020/7/3 上午10:43
 */
@JsonInclude(JsonInclude.Include.NON_NULL)
public class OneJobVO {
    @ApiModelProperty(value = "工作日期")
    private String workDate;
    @JsonIgnore
    private Date workDateSource;
    @ApiModelProperty(value = "工作集合")
    private List<JobVO> jobList;
    @ApiModelProperty(value = "总工作")
    private JobVO total;

    public String getWorkDate() {
        return workDate;
    }

    public void setWorkDate(String workDate) {
        this.workDate = workDate;
    }

    public Date getWorkDateSource() {
        return workDateSource;
    }

    public void setWorkDateSource(Date workDateSource) {
        this.workDateSource = workDateSource;
    }

    public List<JobVO> getJobList() {
        return jobList;
    }

    public void setJobList(List<JobVO> jobList) {
        this.jobList = jobList;
    }

    public JobVO getTotal() {
        return total;
    }

    public void setTotal(JobVO total) {
        this.total = total;
    }
}
