package io.choerodon.agile.api.vo;

import java.math.BigDecimal;

import com.fasterxml.jackson.annotation.JsonInclude;

/**
 * @author jiaxu.cui@hand-china.com 2020/6/30 下午1:46
 */
@JsonInclude(JsonInclude.Include.NON_NULL)
public class UncompletedCountVO {

    private BigDecimal remainingStoryPoints = BigDecimal.ZERO;
    private BigDecimal remainingEstimatedTime = BigDecimal.ZERO;
    private Integer remainingIssueCount = 0;
    private Integer remainingDays = 0;
    private Integer totalDays = 0;
    private Integer totalIssueCount = 0;
    private BigDecimal totalStoryPoints = BigDecimal.ZERO;
    private BigDecimal totalEstimatedTime = BigDecimal.ZERO;

    public BigDecimal getRemainingStoryPoints() {
        return remainingStoryPoints;
    }

    public void setRemainingStoryPoints(BigDecimal remainingStoryPoints) {
        this.remainingStoryPoints = remainingStoryPoints;
    }

    public BigDecimal getRemainingEstimatedTime() {
        return remainingEstimatedTime;
    }

    public void setRemainingEstimatedTime(BigDecimal remainingEstimatedTime) {
        this.remainingEstimatedTime = remainingEstimatedTime;
    }

    public Integer getRemainingIssueCount() {
        return remainingIssueCount;
    }

    public void setRemainingIssueCount(Integer remainingIssueCount) {
        this.remainingIssueCount = remainingIssueCount;
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

    public Integer getTotalIssueCount() {
        return totalIssueCount;
    }

    public void setTotalIssueCount(Integer totalIssueCount) {
        this.totalIssueCount = totalIssueCount;
    }

    public BigDecimal getTotalStoryPoints() {
        return totalStoryPoints;
    }

    public void setTotalStoryPoints(BigDecimal totalStoryPoints) {
        this.totalStoryPoints = totalStoryPoints;
    }

    public BigDecimal getTotalEstimatedTime() {
        return totalEstimatedTime;
    }

    public void setTotalEstimatedTime(BigDecimal totalEstimatedTime) {
        this.totalEstimatedTime = totalEstimatedTime;
    }
}
