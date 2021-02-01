package io.choerodon.agile.api.vo;

/**
 * @author superlee
 * @since 2021-01-20
 */
public class ProjectIssueTypeVO extends ProjectVO {

    private Boolean issueTypeEnabled;

    public Boolean getIssueTypeEnabled() {
        return issueTypeEnabled;
    }

    public void setIssueTypeEnabled(Boolean issueTypeEnabled) {
        this.issueTypeEnabled = issueTypeEnabled;
    }
}
