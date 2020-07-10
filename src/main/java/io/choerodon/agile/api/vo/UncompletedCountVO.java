package io.choerodon.agile.api.vo;

import java.math.BigDecimal;

import com.fasterxml.jackson.annotation.JsonInclude;

/**
 * @author jiaxu.cui@hand-china.com 2020/6/30 下午1:46
 */
@JsonInclude(JsonInclude.Include.NON_NULL)
public class UncompletedCountVO {

    private BigDecimal storyPoints = BigDecimal.ZERO;
    private BigDecimal remainingEstimatedTime = BigDecimal.ZERO;
    private Integer issueCount = 0;
    private Integer remainingDays = 0;
    private Integer totalDays = 0;

    public BigDecimal getStoryPoints() {
        return storyPoints;
    }

    public void setStoryPoints(BigDecimal storyPoints) {
        this.storyPoints = storyPoints;
    }

    public BigDecimal getRemainingEstimatedTime() {
        return remainingEstimatedTime;
    }

    public void setRemainingEstimatedTime(BigDecimal remainingEstimatedTime) {
        this.remainingEstimatedTime = remainingEstimatedTime;
    }

    public Integer getIssueCount() {
        return issueCount;
    }

    public void setIssueCount(Integer issueCount) {
        this.issueCount = issueCount;
    }

    public Integer getRemainingDays() {
        return remainingDays;
    }

    public void setRemainingDays(Integer remainingDays) {
        this.remainingDays = remainingDays;
    }

    public Integer getTotalDays() {
        return totalDays;
    }

    public void setTotalDays(Integer totalDays) {
        this.totalDays = totalDays;
    }
}
