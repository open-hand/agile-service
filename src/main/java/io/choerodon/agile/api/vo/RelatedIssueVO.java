package io.choerodon.agile.api.vo;

import io.swagger.annotations.ApiModelProperty;

import java.util.Set;

/**
 * @author superlee
 * @since 2020-11-16
 */
public class RelatedIssueVO {
    @ApiModelProperty(value = "行")
    private Integer row;
    @ApiModelProperty(value = "问题id")
    private Long issueId;
    @ApiModelProperty(value = "关联的id")
    private Set<Long> relatedIds;
    @ApiModelProperty(value = "关联的行数")
    private Set<Integer> relatedRows;

    public Integer getRow() {
        return row;
    }

    public void setRow(Integer row) {
        this.row = row;
    }

    public Long getIssueId() {
        return issueId;
    }

    public void setIssueId(Long issueId) {
        this.issueId = issueId;
    }

    public Set<Long> getRelatedIds() {
        return relatedIds;
    }

    public void setRelatedIds(Set<Long> relatedIds) {
        this.relatedIds = relatedIds;
    }

    public Set<Integer> getRelatedRows() {
        return relatedRows;
    }

    public void setRelatedRows(Set<Integer> relatedRows) {
        this.relatedRows = relatedRows;
    }
}
