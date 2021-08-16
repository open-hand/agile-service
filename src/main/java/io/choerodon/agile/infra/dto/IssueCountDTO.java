package io.choerodon.agile.infra.dto;

import java.math.BigDecimal;

/**
 * Created by jian_zhang02@163.com on 2018/5/30.
 */
public class IssueCountDTO {
    private Long id;
    private String name;
    private Integer successIssueCount;
    private Integer issueCount;
    private BigDecimal storyPointCount;
    private BigDecimal successStoryPoint;
    private BigDecimal totalStoryPoint;
    private Long issueTypeId;

    private BigDecimal todoStoryPoint;

    private BigDecimal doingStoryPoint;

    private BigDecimal doneStoryPoint;

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public Integer getIssueCount() {
        return issueCount;
    }

    public void setIssueCount(Integer issueCount) {
        this.issueCount = issueCount;
    }

    public void setStoryPointCount(BigDecimal storyPointCount) {
        this.storyPointCount = storyPointCount;
    }

    public BigDecimal getStoryPointCount() {
        return storyPointCount;
    }

    public BigDecimal getSuccessStoryPoint() {
        return successStoryPoint;
    }

    public void setSuccessStoryPoint(BigDecimal successStoryPoint) {
        this.successStoryPoint = successStoryPoint;
    }

    public BigDecimal getTotalStoryPoint() {
        return totalStoryPoint;
    }

    public void setTotalStoryPoint(BigDecimal totalStoryPoint) {
        this.totalStoryPoint = totalStoryPoint;
    }

    public Long getIssueTypeId() {
        return issueTypeId;
    }

    public void setIssueTypeId(Long issueTypeId) {
        this.issueTypeId = issueTypeId;
    }

    public BigDecimal getTodoStoryPoint() {
        return todoStoryPoint;
    }

    public void setTodoStoryPoint(BigDecimal todoStoryPoint) {
        this.todoStoryPoint = todoStoryPoint;
    }

    public BigDecimal getDoingStoryPoint() {
        return doingStoryPoint;
    }

    public void setDoingStoryPoint(BigDecimal doingStoryPoint) {
        this.doingStoryPoint = doingStoryPoint;
    }

    public BigDecimal getDoneStoryPoint() {
        return doneStoryPoint;
    }

    public void setDoneStoryPoint(BigDecimal doneStoryPoint) {
        this.doneStoryPoint = doneStoryPoint;
    }

    public Integer getSuccessIssueCount() {
        return successIssueCount;
    }

    public void setSuccessIssueCount(Integer successIssueCount) {
        this.successIssueCount = successIssueCount;
    }
}
