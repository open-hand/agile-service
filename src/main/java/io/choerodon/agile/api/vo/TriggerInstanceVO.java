package io.choerodon.agile.api.vo;

/**
 * @author huaxin.deng@hand-china.com 2021-05-10 19:32:44
 */
public class TriggerInstanceVO {

    private Long projectId;

    private Long paramIssueId;

    private Long paramOpenIssueId;

    private String paramName;

    private Long paramBacklogId;

    private String paramBacklogName;

    private String typeCode;

    private String summary;

    public TriggerInstanceVO(Long projectId, Long paramIssueId, Long paramOpenIssueId, String paramName, String typeCode, String summary) {
        this.projectId = projectId;
        this.paramIssueId = paramIssueId;
        this.paramOpenIssueId = paramOpenIssueId;
        this.paramName = paramName;
        this.typeCode = typeCode;
        this.summary = summary;
    }

    public TriggerInstanceVO(Long projectId, Long paramBacklogId, String paramBacklogName, String typeCode, String summary) {
        this.projectId = projectId;
        this.paramBacklogId = paramBacklogId;
        this.paramBacklogName = paramBacklogName;
        this.typeCode = typeCode;
        this.summary = summary;
    }

    public Long getProjectId() {
        return projectId;
    }

    public void setProjectId(Long projectId) {
        this.projectId = projectId;
    }

    public Long getParamIssueId() {
        return paramIssueId;
    }

    public void setParamIssueId(Long paramIssueId) {
        this.paramIssueId = paramIssueId;
    }

    public Long getParamOpenIssueId() {
        return paramOpenIssueId;
    }

    public void setParamOpenIssueId(Long paramOpenIssueId) {
        this.paramOpenIssueId = paramOpenIssueId;
    }

    public Long getParamBacklogId() {
        return paramBacklogId;
    }

    public void setParamBacklogId(Long paramBacklogId) {
        this.paramBacklogId = paramBacklogId;
    }

    public String getParamName() {
        return paramName;
    }

    public void setParamName(String paramName) {
        this.paramName = paramName;
    }

    public String getParamBacklogName() {
        return paramBacklogName;
    }

    public void setParamBacklogName(String paramBacklogName) {
        this.paramBacklogName = paramBacklogName;
    }

    public String getTypeCode() {
        return typeCode;
    }

    public void setTypeCode(String typeCode) {
        this.typeCode = typeCode;
    }

    public String getSummary() {
        return summary;
    }

    public void setSummary(String summary) {
        this.summary = summary;
    }
}
