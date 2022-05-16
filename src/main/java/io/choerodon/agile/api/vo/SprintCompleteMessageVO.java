package io.choerodon.agile.api.vo;

import io.swagger.annotations.ApiModelProperty;

import java.util.List;

/**
 * Created by jian_zhang02@163.com on 2018/5/18.
 */
public class SprintCompleteMessageVO {
    @ApiModelProperty(value = "未完成问题")
    private Integer incompleteIssues;
    @ApiModelProperty(value = "部分完成的问题")
    private Integer partiallyCompleteIssues;
    @ApiModelProperty(value = "冲刺名称")
    private List<SprintNameVO> sprintNames;
    @ApiModelProperty(value = "父级已完成的子任务")
    private List<IssueNumVO> parentsDoneUnfinishedSubtasks;

    public Integer getIncompleteIssues() {
        return incompleteIssues;
    }

    public void setIncompleteIssues(Integer incompleteIssues) {
        this.incompleteIssues = incompleteIssues;
    }

    public Integer getPartiallyCompleteIssues() {
        return partiallyCompleteIssues;
    }

    public void setPartiallyCompleteIssues(Integer partiallyCompleteIssues) {
        this.partiallyCompleteIssues = partiallyCompleteIssues;
    }

    public List<SprintNameVO> getSprintNames() {
        return sprintNames;
    }

    public void setSprintNames(List<SprintNameVO> sprintNames) {
        this.sprintNames = sprintNames;
    }

    public List<IssueNumVO> getParentsDoneUnfinishedSubtasks() {
        return parentsDoneUnfinishedSubtasks;
    }

    public void setParentsDoneUnfinishedSubtasks(List<IssueNumVO> parentsDoneUnfinishedSubtasks) {
        this.parentsDoneUnfinishedSubtasks = parentsDoneUnfinishedSubtasks;
    }
}
