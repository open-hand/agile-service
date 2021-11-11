package io.choerodon.agile.api.vo;

/**
 * @author zhaotianxin
 * @date 2021-11-10 11:35
 */
public class WorkHoursCountVO {
    private Integer registerWorkTimeUserCount;

    private Integer unsaturatedUserCount;

    private Integer unsaturatedTimes;

    public WorkHoursCountVO() {
    }

    public WorkHoursCountVO(Integer unsaturatedUserCount, Integer unsaturatedTimes) {
        this.unsaturatedUserCount = unsaturatedUserCount;
        this.unsaturatedTimes = unsaturatedTimes;
    }

    public Integer getRegisterWorkTimeUserCount() {
        return registerWorkTimeUserCount;
    }

    public void setRegisterWorkTimeUserCount(Integer registerWorkTimeUserCount) {
        this.registerWorkTimeUserCount = registerWorkTimeUserCount;
    }

    public Integer getUnsaturatedUserCount() {
        return unsaturatedUserCount;
    }

    public void setUnsaturatedUserCount(Integer unsaturatedUserCount) {
        this.unsaturatedUserCount = unsaturatedUserCount;
    }

    public Integer getUnsaturatedTimes() {
        return unsaturatedTimes;
    }

    public void setUnsaturatedTimes(Integer unsaturatedTimes) {
        this.unsaturatedTimes = unsaturatedTimes;
    }
}
