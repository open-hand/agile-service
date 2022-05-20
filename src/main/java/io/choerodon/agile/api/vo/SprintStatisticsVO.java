package io.choerodon.agile.api.vo;

import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

/**
 * @author jiaxu.cui@hand-china.com 2020/7/1 下午1:57
 */
public class SprintStatisticsVO {
    /**
     * issue总数，包括故事，任务，子任务，缺陷
     */
    @ApiModelProperty(value = "issue总数")
    private Integer total;

    /**
     * 完成数量
     */
    @ApiModelProperty(value = "完成数量")
    private IssueCountWithStatusIdsVO completedCount;
    /**
     * 未完成数量
     */
    @ApiModelProperty(value = "未完成数量")
    private IssueCountWithStatusIdsVO uncompletedCount;
    /**
     * 待办数量
     */
    @ApiModelProperty(value = "待办数量")
    private IssueCountWithStatusIdsVO todoCount;
    /**
     * 未分配数量
     */
    @ApiModelProperty(value = "未分配数量")
    private IssueCountWithStatusIdsVO unassignCount;

    @Encrypt
    @ApiModelProperty(value = "冲刺id")
    private Long sprintId;

    public Integer getTotal() {
        return total;
    }

    public void setTotal(Integer total) {
        this.total = total;
    }

    public IssueCountWithStatusIdsVO getCompletedCount() {
        return completedCount;
    }

    public void setCompletedCount(IssueCountWithStatusIdsVO completedCount) {
        this.completedCount = completedCount;
    }

    public IssueCountWithStatusIdsVO getUncompletedCount() {
        return uncompletedCount;
    }

    public void setUncompletedCount(IssueCountWithStatusIdsVO uncompletedCount) {
        this.uncompletedCount = uncompletedCount;
    }

    public IssueCountWithStatusIdsVO getTodoCount() {
        return todoCount;
    }

    public void setTodoCount(IssueCountWithStatusIdsVO todoCount) {
        this.todoCount = todoCount;
    }

    public IssueCountWithStatusIdsVO getUnassignCount() {
        return unassignCount;
    }

    public void setUnassignCount(IssueCountWithStatusIdsVO unassignCount) {
        this.unassignCount = unassignCount;
    }

    public Long getSprintId() {
        return sprintId;
    }

    public void setSprintId(Long sprintId) {
        this.sprintId = sprintId;
    }
}
