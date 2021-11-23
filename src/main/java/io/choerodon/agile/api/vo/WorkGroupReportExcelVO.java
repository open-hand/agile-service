package io.choerodon.agile.api.vo;

import java.math.BigDecimal;
import java.util.Set;

/**
 * @author zhaotianxin
 * @date 2021-11-11 10:42
 */
public class WorkGroupReportExcelVO {
    private Long id;

    private Long parentId;

    private String workGroupName;

    private Integer userCount;

    private Integer actualUserCount;

    private String actualUserProportion;

    private Integer unsaturatedUserCount;

    private String unsaturatedUserProportion;

    private Integer unsaturatedTimes;

    private Set<Long> userIds;

    public WorkGroupReportExcelVO() {
    }

    public WorkGroupReportExcelVO(Long id, Long parentId, String workGroupName, Integer userCount, Set<Long> userIds) {
        this.id = id;
        this.parentId = parentId;
        this.workGroupName = workGroupName;
        this.userCount = userCount;
        this.userIds = userIds;
    }

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public Long getParentId() {
        return parentId;
    }

    public void setParentId(Long parentId) {
        this.parentId = parentId;
    }

    public String getWorkGroupName() {
        return workGroupName;
    }

    public void setWorkGroupName(String workGroupName) {
        this.workGroupName = workGroupName;
    }

    public Integer getUserCount() {
        return userCount;
    }

    public void setUserCount(Integer userCount) {
        this.userCount = userCount;
    }

    public Integer getActualUserCount() {
        return actualUserCount;
    }

    public void setActualUserCount(Integer actualUserCount) {
        this.actualUserCount = actualUserCount;
    }


    public Integer getUnsaturatedUserCount() {
        return unsaturatedUserCount;
    }

    public void setUnsaturatedUserCount(Integer unsaturatedUserCount) {
        this.unsaturatedUserCount = unsaturatedUserCount;
    }

    public String getActualUserProportion() {
        return actualUserProportion;
    }

    public void setActualUserProportion(String actualUserProportion) {
        this.actualUserProportion = actualUserProportion;
    }

    public String getUnsaturatedUserProportion() {
        return unsaturatedUserProportion;
    }

    public void setUnsaturatedUserProportion(String unsaturatedUserProportion) {
        this.unsaturatedUserProportion = unsaturatedUserProportion;
    }

    public Integer getUnsaturatedTimes() {
        return unsaturatedTimes;
    }

    public void setUnsaturatedTimes(Integer unsaturatedTimes) {
        this.unsaturatedTimes = unsaturatedTimes;
    }

    public Set<Long> getUserIds() {
        return userIds;
    }

    public void setUserIds(Set<Long> userIds) {
        this.userIds = userIds;
    }
}
