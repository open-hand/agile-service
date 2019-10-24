package io.choerodon.agile.api.vo;

import java.util.Date;

/**
 * Created by HuangFuqiang@choerodon.io on 2019/9/27.
 * Email: fuqianghuang01@gmail.com
 */
public class SprintCreateVO {

    private String sprintName;

    private String sprintGoal;

    private Date startDate;

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
