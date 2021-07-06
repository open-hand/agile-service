package io.choerodon.agile.api.vo;

import java.util.Set;

/**
 * @author superlee
 * @since 2021-06-25
 */
public class IssueWithBranchVO {

    private Long issueId;

    private Set<Long> branchIds;

    public Long getIssueId() {
        return issueId;
    }

    public void setIssueId(Long issueId) {
        this.issueId = issueId;
    }

    public Set<Long> getBranchIds() {
        return branchIds;
    }

    public void setBranchIds(Set<Long> branchIds) {
        this.branchIds = branchIds;
    }
}
