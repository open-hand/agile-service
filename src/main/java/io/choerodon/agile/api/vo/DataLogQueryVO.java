package io.choerodon.agile.api.vo;

import org.hzero.starter.keyencrypt.core.Encrypt;

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
    @Encrypt
    private List<Long> typeIds;
    @Encrypt
    private List<Long> createdByIds;
    private List<String> otherTypes;
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

    public List<String> getOtherTypes() {
        return otherTypes;
    }

    public void setOtherTypes(List<String> otherTypes) {
        this.otherTypes = otherTypes;
    }
}
