package io.choerodon.agile.api.vo.business;

/**
 * @author huaxin.deng@hand-china.com 2021-12-21 19:06:56
 */
public class IssueBacklogRelVO {

    private Long issueId;

    private Long backlogId;

    private String summary;

    private String backlogNum;

    private String source;

    public Long getIssueId() {
        return issueId;
    }

    public void setIssueId(Long issueId) {
        this.issueId = issueId;
    }

    public Long getBacklogId() {
        return backlogId;
    }

    public void setBacklogId(Long backlogId) {
        this.backlogId = backlogId;
    }

    public String getSummary() {
        return summary;
    }

    public void setSummary(String summary) {
        this.summary = summary;
    }

    public String getBacklogNum() {
        return backlogNum;
    }

    public void setBacklogNum(String backlogNum) {
        this.backlogNum = backlogNum;
    }

    public String getSource() {
        return source;
    }

    public void setSource(String source) {
        this.source = source;
    }
}
