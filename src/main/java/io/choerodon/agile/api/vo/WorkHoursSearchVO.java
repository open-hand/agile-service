package io.choerodon.agile.api.vo;

import org.hzero.starter.keyencrypt.core.Encrypt;

import java.util.Date;
import java.util.List;

/**
 * @author zhaotianxin
 * @date 2021-10-15 16:26
 */
public class WorkHoursSearchVO {

    private Date startTime;

    private Date endTime;

    @Encrypt
    private List<Long> userIds;

    private List<Long> projectIds;

    private Boolean exportMonthlyReport;

    @Encrypt
    private List<Long> workGroupIds;

    public Date getStartTime() {
        return startTime;
    }

    public void setStartTime(Date startTime) {
        this.startTime = startTime;
    }

    public Date getEndTime() {
        return endTime;
    }

    public void setEndTime(Date endTime) {
        this.endTime = endTime;
    }

    public List<Long> getUserIds() {
        return userIds;
    }

    public void setUserIds(List<Long> userIds) {
        this.userIds = userIds;
    }

    public List<Long> getProjectIds() {
        return projectIds;
    }

    public void setProjectIds(List<Long> projectIds) {
        this.projectIds = projectIds;
    }

    public Boolean getExportMonthlyReport() {
        return exportMonthlyReport;
    }

    public void setExportMonthlyReport(Boolean exportMonthlyReport) {
        this.exportMonthlyReport = exportMonthlyReport;
    }

    public List<Long> getWorkGroupIds() {
        return workGroupIds;
    }

    public void setWorkGroupIds(List<Long> workGroupIds) {
        this.workGroupIds = workGroupIds;
    }
}
