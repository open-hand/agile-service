package io.choerodon.agile.api.vo;

import java.math.BigDecimal;

import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.annotations.ApiModelProperty;

/**
 * @author jiaxu.cui@hand-china.com 2020/6/30 下午1:46
 */
@JsonInclude(JsonInclude.Include.NON_NULL)
public class UncompletedCountVO {

    @ApiModelProperty(value = "剩余故事点")
    private BigDecimal remainingStoryPoints = BigDecimal.ZERO;
    @ApiModelProperty(value = "剩余预计时间")
    private BigDecimal remainingEstimatedTime = BigDecimal.ZERO;
    @ApiModelProperty(value = "剩余问题数量")
    private Integer remainingIssueCount = 0;
    @ApiModelProperty(value = "剩余天数")
    private Integer remainingDays = 0;
    @ApiModelProperty(value = "总天数")
    private Integer totalDays = 0;
    @ApiModelProperty(value = "总问题数")
    private Integer totalIssueCount = 0;
    @ApiModelProperty(value = "总故事点")
    private BigDecimal totalStoryPoints = BigDecimal.ZERO;
    @ApiModelProperty(value = "总剩余时间")
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
