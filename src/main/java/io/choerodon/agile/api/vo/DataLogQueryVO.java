package io.choerodon.agile.api.vo;

import java.util.Date;
import java.util.List;
import javax.validation.constraints.NotNull;

/**
 * @author chihao.ran@hand-china.com
 * 2021/03/22 14:20
 */
public class DataLogQueryVO {
    @NotNull(message = "error.startDate.not.null")
    private Date startDate;
    private Date endDate;
    private List<Long> typeIds;
    private List<Long> createdByIds;
    private Boolean containBackLog;

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

    public List<Long> getTypeIds() {
        return typeIds;
    }

    public void setTypeIds(List<Long> typeIds) {
        this.typeIds = typeIds;
    }

    public List<Long> getCreatedByIds() {
        return createdByIds;
    }

    public void setCreatedByIds(List<Long> createdByIds) {
        this.createdByIds = createdByIds;
    }

    public Boolean getContainBackLog() {
        return containBackLog;
    }

    public void setContainBackLog(Boolean containBackLog) {
        this.containBackLog = containBackLog;
    }
}
