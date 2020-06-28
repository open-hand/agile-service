package io.choerodon.agile.api.vo;

import io.choerodon.agile.infra.constants.EncryptionConstant;
import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

import java.util.List;

/**
 * Created by jian_zhang02@163.com on 2018/5/28.
 */
public class MoveIssueVO {

    @ApiModelProperty(value = "移动的问题集合")
    @Encrypt/*(EncryptionConstant.AGILE_ISSUE)*/
    private List<Long> issueIds;

    @ApiModelProperty(value = "是否在前面")
    private Boolean isBefore;

    @ApiModelProperty(value = "移动的参照问题id")
    @Encrypt(ignoreValue = {"0"})/*(EncryptionConstant.AGILE_ISSUE)*/
    private Long outsetIssueId;

    @ApiModelProperty(value = "是否生成移动日志")
    private Boolean rankIndex;
    @Encrypt(ignoreValue = {"0"})/*(EncryptionConstant.FD_STATUS)*/
    private Long updateStatusId;

    private String statusCategoryCode;

    public void setUpdateStatusId(Long updateStatusId) {
        this.updateStatusId = updateStatusId;
    }

    public Long getUpdateStatusId() {
        return updateStatusId;
    }

    public void setStatusCategoryCode(String statusCategoryCode) {
        this.statusCategoryCode = statusCategoryCode;
    }

    public String getStatusCategoryCode() {
        return statusCategoryCode;
    }

    public Boolean getRankIndex() {
        return rankIndex;
    }

    public void setRankIndex(Boolean rankIndex) {
        this.rankIndex = rankIndex;
    }

    public List<Long> getIssueIds() {
        return issueIds;
    }

    public void setIssueIds(List<Long> issueIds) {
        this.issueIds = issueIds;
    }

    public Boolean getBefore() {
        return isBefore;
    }

    public void setBefore(Boolean before) {
        isBefore = before;
    }

    public Long getOutsetIssueId() {
        return outsetIssueId;
    }

    public void setOutsetIssueId(Long outsetIssueId) {
        this.outsetIssueId = outsetIssueId;
    }
}
