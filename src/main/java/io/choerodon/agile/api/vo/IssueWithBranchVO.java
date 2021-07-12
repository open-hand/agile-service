package io.choerodon.agile.api.vo;

import java.util.List;

/**
 * @author superlee
 * @since 2021-06-25
 */
public class IssueWithBranchVO {

    private Long issueId;

    private List<BranchVO> branches;

    public Long getIssueId() {
        return issueId;
    }

    public void setIssueId(Long issueId) {
        this.issueId = issueId;
    }

    public List<BranchVO> getBranches() {
        return branches;
    }

    public void setBranches(List<BranchVO> branches) {
        this.branches = branches;
    }
}
