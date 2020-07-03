package io.choerodon.agile.api.vo;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonInclude;
import org.hzero.core.base.BaseConstants;

/**
 * @author jiaxu.cui@hand-china.com 2020/7/3 上午10:46
 */
@JsonInclude(JsonInclude.Include.NON_NULL)
public class JobVO {

    public JobVO(List<JobVO> jobList) {
        this.bugCreatedCount = jobList.stream().map(JobVO::getBugCreatedCount).reduce(BaseConstants.Digital.ZERO, Integer::sum);
        this.bugFixCount = jobList.stream().map(JobVO::getBugFixCount).reduce(BaseConstants.Digital.ZERO, Integer::sum);
        this.taskCount = jobList.stream().map(JobVO::getTaskCount).reduce(BaseConstants.Digital.ZERO, Integer::sum);
        this.storyCount = jobList.stream().map(JobVO::getStoryCount).reduce(BaseConstants.Digital.ZERO, Integer::sum);
        this.storyPointCount = jobList.stream().map(JobVO::getStoryPointCount).reduce(BaseConstants.Digital.ZERO, Integer::sum);
        this.workTime = jobList.stream().map(JobVO::getWorkTime).reduce(BaseConstants.Digital.ZERO, Integer::sum);
    }

    public JobVO() {
    }

    private String worker;
    private Integer taskCount;
    private Integer storyCount;
    private Integer storyPointCount;
    private Integer bugCreatedCount;
    private Integer bugFixCount;
    private Integer workTime;

    public String getWorker() {
        return worker;
    }

    public void setWorker(String worker) {
        this.worker = worker;
    }

    public Integer getTaskCount() {
        return taskCount;
    }

    public void setTaskCount(Integer taskCount) {
        this.taskCount = taskCount;
    }

    public Integer getStoryCount() {
        return storyCount;
    }

    public void setStoryCount(Integer storyCount) {
        this.storyCount = storyCount;
    }

    public Integer getStoryPointCount() {
        return storyPointCount;
    }

    public void setStoryPointCount(Integer storyPointCount) {
        this.storyPointCount = storyPointCount;
    }

    public Integer getBugCreatedCount() {
        return bugCreatedCount;
    }

    public void setBugCreatedCount(Integer bugCreatedCount) {
        this.bugCreatedCount = bugCreatedCount;
    }

    public Integer getBugFixCount() {
        return bugFixCount;
    }

    public void setBugFixCount(Integer bugFixCount) {
        this.bugFixCount = bugFixCount;
    }

    public Integer getWorkTime() {
        return workTime;
    }

    public void setWorkTime(Integer workTime) {
        this.workTime = workTime;
    }
}
