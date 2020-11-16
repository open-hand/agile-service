package io.choerodon.agile.api.vo;

import java.util.Set;

/**
 * @author superlee
 * @since 2020-11-16
 */
public class RelatedIssueVO {

    private Integer row;

    private Long issueId;

    private Set<Long> relatedIds;

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
