package io.choerodon.agile.api.vo;

/**
 * @author jiaxu.cui@hand-china.com 2020/7/1 下午1:57
 */
public class SprintStatisticsVO {
    /**
     * issue总数，包括故事，任务，子任务，缺陷
     */
    private Integer total;

    /**
     * 完成数量
     */
    private Integer completedCount;
    /**
     * 未完成数量
     */
    private Integer uncompletedCount;
    /**
     * 待办数量
     */
    private Integer todoCount;
    /**
     * 未分配数量
     */
    private Integer unassignCount;

    public Integer getTotal() {
        return total;
    }

    public void setTotal(Integer total) {
        this.total = total;
    }

    public Integer getCompletedCount() {
        return completedCount;
    }

    public void setCompletedCount(Integer completedCount) {
        this.completedCount = completedCount;
    }

    public Integer getUncompletedCount() {
        return uncompletedCount;
    }

    public void setUncompletedCount(Integer uncompletedCount) {
        this.uncompletedCount = uncompletedCount;
    }

    public Integer getTodoCount() {
        return todoCount;
    }

    public void setTodoCount(Integer todoCount) {
        this.todoCount = todoCount;
    }

    public Integer getUnassignCount() {
        return unassignCount;
    }

    public void setUnassignCount(Integer unassignCount) {
        this.unassignCount = unassignCount;
    }
}
