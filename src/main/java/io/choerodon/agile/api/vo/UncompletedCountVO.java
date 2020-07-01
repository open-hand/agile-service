package io.choerodon.agile.api.vo;

import java.math.BigDecimal;

/**
 * @author jiaxu.cui@hand-china.com 2020/6/30 下午1:46
 */
public class UncompletedCountVO {

    private BigDecimal storyPoints;
    private BigDecimal remainingEstimatedTime;
    private BigDecimal issueCount;
    private Integer remainingDays;
    private Integer totalDays;

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

    public BigDecimal getIssueCount() {
        return issueCount;
    }

    public void setIssueCount(BigDecimal issueCount) {
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
