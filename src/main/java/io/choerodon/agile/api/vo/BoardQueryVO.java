package io.choerodon.agile.api.vo;

import java.util.List;

import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

/**
 * @author jiaxu.cui@hand-china.com 2020/8/7 下午3:43
 */
public class BoardQueryVO {

    @ApiModelProperty("personal filter")
    @Encrypt
    private List<Long> personalFilterIds;
    @ApiModelProperty("quick filter")
    @Encrypt
    private List<Long> quickFilterIds;
    @ApiModelProperty("priority id")
    @Encrypt
    private List<Long> priorityIds;
    @ApiModelProperty("经办人搜索")
    @Encrypt
    private List<Long> assigneeFilterIds;
    @ApiModelProperty("search item，my problem")
    @Encrypt
    private Long assigneeId;
    @ApiModelProperty("search item，only story")
    private Boolean onlyStory;
    @ApiModelProperty("冲刺id")
    @Encrypt
    private  Long sprintId;
    @ApiModelProperty("星标")
    private Boolean starBeacon;

    public Boolean getStarBeacon() {
        return starBeacon;
    }

    public void setStarBeacon(Boolean starBeacon) {
        this.starBeacon = starBeacon;
    }

    public List<Long> getPersonalFilterIds() {
        return personalFilterIds;
    }

    public void setPersonalFilterIds(List<Long> personalFilterIds) {
        this.personalFilterIds = personalFilterIds;
    }

    public List<Long> getQuickFilterIds() {
        return quickFilterIds;
    }

    public void setQuickFilterIds(List<Long> quickFilterIds) {
        this.quickFilterIds = quickFilterIds;
    }

    public List<Long> getPriorityIds() {
        return priorityIds;
    }

    public void setPriorityIds(List<Long> priorityIds) {
        this.priorityIds = priorityIds;
    }

    public List<Long> getAssigneeFilterIds() {
        return assigneeFilterIds;
    }

    public void setAssigneeFilterIds(List<Long> assigneeFilterIds) {
        this.assigneeFilterIds = assigneeFilterIds;
    }

    public Long getAssigneeId() {
        return assigneeId;
    }

    public void setAssigneeId(Long assigneeId) {
        this.assigneeId = assigneeId;
    }

    public Boolean getOnlyStory() {
        return onlyStory;
    }

    public void setOnlyStory(Boolean onlyStory) {
        this.onlyStory = onlyStory;
    }

    public Long getSprintId() {
        return sprintId;
    }

    public void setSprintId(Long sprintId) {
        this.sprintId = sprintId;
    }
}
