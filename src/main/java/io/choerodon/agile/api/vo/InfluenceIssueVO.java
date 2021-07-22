package io.choerodon.agile.api.vo;

import java.util.List;

/**
 * @author zhaotianxin
 * @date 2021-07-13 15:34
 */
public class InfluenceIssueVO {
    private Long issueId;

    private Long statusId;

    private List<InfluenceIssueVO> childrenVO;

    public Long getIssueId() {
        return issueId;
    }

    public void setIssueId(Long issueId) {
        this.issueId = issueId;
    }

    public Long getStatusId() {
        return statusId;
    }

    public void setStatusId(Long statusId) {
        this.statusId = statusId;
    }

    public List<InfluenceIssueVO> getChildrenVO() {
        return childrenVO;
    }

    public void setChildrenVO(List<InfluenceIssueVO> childrenVO) {
        this.childrenVO = childrenVO;
    }
}
