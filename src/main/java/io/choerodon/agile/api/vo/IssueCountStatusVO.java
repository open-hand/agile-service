package io.choerodon.agile.api.vo;

/**
 * @author chihao.ran@hand-china.com
 * 2021/08/12 19:27
 */
public class IssueCountStatusVO {
    private Long statusId;
    private Long issueCount;

    public Long getStatusId() {
        return statusId;
    }

    public void setStatusId(Long statusId) {
        this.statusId = statusId;
    }

    public Long getIssueCount() {
        return issueCount;
    }

    public void setIssueCount(Long issueCount) {
        this.issueCount = issueCount;
    }
}
