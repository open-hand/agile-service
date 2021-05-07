package io.choerodon.agile.api.vo;

import io.choerodon.agile.api.vo.business.IssueVO;

import java.util.List;

/**
 * @author superlee
 * @since 2021-04-14
 */
public class TagWithIssueVO extends TagVO {

    private List<IssueVO> issues;

    public List<IssueVO> getIssues() {
        return issues;
    }

    public void setIssues(List<IssueVO> issues) {
        this.issues = issues;
    }
}
