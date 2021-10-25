package io.choerodon.agile.api.vo;

import java.math.BigDecimal;
import java.util.List;

/**
 * @author zhaotianxin
 * @date 2021-10-20 16:46
 */
public class WorkHoursCalendarCountVO {

    private BigDecimal count;

    List<WorkHoursLogVO> workHoursLogVOList;

    public BigDecimal getCount() {
        return count;
    }

    public void setCount(BigDecimal count) {
        this.count = count;
    }

    public List<WorkHoursLogVO> getWorkHoursLogVOList() {
        return workHoursLogVOList;
    }

    public void setWorkHoursLogVOList(List<WorkHoursLogVO> workHoursLogVOList) {
        this.workHoursLogVOList = workHoursLogVOList;
    }
}
