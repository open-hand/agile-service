package io.choerodon.agile.api.vo;

/**
 * @author superlee
 * @since 2021-01-19
 */
public class IssueTypeCountVO {

    private Long issueTypeId;

    private Integer count;

    public Long getIssueTypeId() {
        return issueTypeId;
    }

    public void setIssueTypeId(Long issueTypeId) {
        this.issueTypeId = issueTypeId;
    }

    public Integer getCount() {
        return count;
    }

    public void setCount(Integer count) {
        this.count = count;
    }
}
