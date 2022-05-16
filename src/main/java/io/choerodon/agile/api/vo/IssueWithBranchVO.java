package io.choerodon.agile.api.vo;

import io.swagger.annotations.ApiModelProperty;

import java.util.List;

/**
 * @author superlee
 * @since 2021-06-25
 */
public class IssueWithBranchVO {
    @ApiModelProperty(value = "问题id")
    private Long issueId;
    @ApiModelProperty(value = "分支")
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
