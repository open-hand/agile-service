package io.choerodon.agile.api.vo;


import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;

/**
 * @author shinan.chen
 * @since 2019/4/2
 */
public class AdjustOrderVO {
    @ApiModelProperty(value = "是否拖动到第一个")
    @NotNull(message = "error.field.before.null")
    private Boolean before;
    @ApiModelProperty(value = "当前移动的字段id")
    @Encrypt
    private Long currentFieldId;
    @ApiModelProperty(value = "before：true，在当前移动的值之后，false，在当前移动的值之前")
    @Encrypt
    private Long outsetFieldId;
    @NotEmpty(message = "error.field.issueType.null")
    @ApiModelProperty(value = "问题类型")
    private String issueType;
    @ApiModelProperty(value = "前一个rank值")
    private String previousRank;
    @ApiModelProperty(value = "后一个rank值")
    private String nextRank;

    public String getPreviousRank() {
        return previousRank;
    }

    public void setPreviousRank(String previousRank) {
        this.previousRank = previousRank;
    }

    public String getNextRank() {
        return nextRank;
    }

    public void setNextRank(String nextRank) {
        this.nextRank = nextRank;
    }

    public String getIssueType() {
        return issueType;
    }

    public void setIssueType(String issueType) {
        this.issueType = issueType;
    }

    public Boolean getBefore() {
        return before;
    }

    public void setBefore(Boolean before) {
        this.before = before;
    }

    public Long getCurrentFieldId() {
        return currentFieldId;
    }

    public void setCurrentFieldId(Long currentFieldId) {
        this.currentFieldId = currentFieldId;
    }

    public Long getOutsetFieldId() {
        return outsetFieldId;
    }

    public void setOutsetFieldId(Long outsetFieldId) {
        this.outsetFieldId = outsetFieldId;
    }
}
