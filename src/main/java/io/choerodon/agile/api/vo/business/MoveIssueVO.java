//
// Source code recreated from a .class file by IntelliJ IDEA
// (powered by Fernflower decompiler)
//

package io.choerodon.agile.api.vo.business;



import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

import java.util.List;

public class MoveIssueVO {
    @ApiModelProperty("移动的问题集合")
    @Encrypt
    private List<Long> issueIds;
    @ApiModelProperty("是否在前面")
    private Boolean isBefore;
    @ApiModelProperty("移动的参照问题id")
    @Encrypt(ignoreValue = {"0"})
    private Long outsetIssueId;
    @ApiModelProperty("是否生成移动日志")
    private Boolean rankIndex;
    @Encrypt
    @ApiModelProperty("更新状态id")
    private Long updateStatusId;
    @ApiModelProperty("状态类别编码")
    private String statusCategoryCode;
    @Encrypt(ignoreValue = {"0"})
    @ApiModelProperty("是否生成移动日志")
    private Long currentPiId;

    public void setUpdateStatusId(Long updateStatusId) {
        this.updateStatusId = updateStatusId;
    }

    public Long getUpdateStatusId() {
        return this.updateStatusId;
    }

    public void setStatusCategoryCode(String statusCategoryCode) {
        this.statusCategoryCode = statusCategoryCode;
    }

    public String getStatusCategoryCode() {
        return this.statusCategoryCode;
    }

    public Boolean getRankIndex() {
        return this.rankIndex;
    }

    public void setRankIndex(Boolean rankIndex) {
        this.rankIndex = rankIndex;
    }

    public List<Long> getIssueIds() {
        return this.issueIds;
    }

    public void setIssueIds(List<Long> issueIds) {
        this.issueIds = issueIds;
    }

    public Boolean getBefore() {
        return this.isBefore;
    }

    public void setBefore(Boolean before) {
        this.isBefore = before;
    }

    public Long getOutsetIssueId() {
        return this.outsetIssueId;
    }

    public void setOutsetIssueId(Long outsetIssueId) {
        this.outsetIssueId = outsetIssueId;
    }

    public Long getCurrentPiId() {
        return currentPiId;
    }

    public void setCurrentPiId(Long currentPiId) {
        this.currentPiId = currentPiId;
    }
}
