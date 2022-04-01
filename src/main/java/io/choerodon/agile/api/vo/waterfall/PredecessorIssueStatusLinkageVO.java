package io.choerodon.agile.api.vo.waterfall;

import io.choerodon.agile.api.vo.IssueTypeVO;
import io.choerodon.agile.api.vo.StatusVO;
import org.hzero.starter.keyencrypt.core.Encrypt;

/**
 * @author huaxin.deng@hand-china.com
 * @since 2022/4/1
 */
public class PredecessorIssueStatusLinkageVO {
    @Encrypt
    private Long id;

    @Encrypt
    private Long issueTypeId;

    @Encrypt
    private Long statusId;

    private String predecessorType;

    @Encrypt
    private Long predecessorIssueTypeId;

    private IssueTypeVO predecessorIssueType;

    @Encrypt
    private Long predecessorIssueStatusId;

    private StatusVO predecessorIssueStatus;

    private StatusVO statusVO;

    private IssueTypeVO issueTypeVO;

    private Long projectId;

    private Long organizationId;

    private Boolean isTriggered;

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public Long getIssueTypeId() {
        return issueTypeId;
    }

    public void setIssueTypeId(Long issueTypeId) {
        this.issueTypeId = issueTypeId;
    }

    public Long getStatusId() {
        return statusId;
    }

    public void setStatusId(Long statusId) {
        this.statusId = statusId;
    }

    public String getPredecessorType() {
        return predecessorType;
    }

    public void setPredecessorType(String predecessorType) {
        this.predecessorType = predecessorType;
    }

    public Long getPredecessorIssueTypeId() {
        return predecessorIssueTypeId;
    }

    public void setPredecessorIssueTypeId(Long predecessorIssueTypeId) {
        this.predecessorIssueTypeId = predecessorIssueTypeId;
    }

    public IssueTypeVO getPredecessorIssueType() {
        return predecessorIssueType;
    }

    public void setPredecessorIssueType(IssueTypeVO predecessorIssueType) {
        this.predecessorIssueType = predecessorIssueType;
    }

    public Long getPredecessorIssueStatusId() {
        return predecessorIssueStatusId;
    }

    public void setPredecessorIssueStatusId(Long predecessorIssueStatusId) {
        this.predecessorIssueStatusId = predecessorIssueStatusId;
    }

    public StatusVO getPredecessorIssueStatus() {
        return predecessorIssueStatus;
    }

    public void setPredecessorIssueStatus(StatusVO predecessorIssueStatus) {
        this.predecessorIssueStatus = predecessorIssueStatus;
    }

    public StatusVO getStatusVO() {
        return statusVO;
    }

    public void setStatusVO(StatusVO statusVO) {
        this.statusVO = statusVO;
    }

    public IssueTypeVO getIssueTypeVO() {
        return issueTypeVO;
    }

    public void setIssueTypeVO(IssueTypeVO issueTypeVO) {
        this.issueTypeVO = issueTypeVO;
    }

    public Long getProjectId() {
        return projectId;
    }

    public void setProjectId(Long projectId) {
        this.projectId = projectId;
    }

    public Long getOrganizationId() {
        return organizationId;
    }

    public void setOrganizationId(Long organizationId) {
        this.organizationId = organizationId;
    }

    public Boolean getTriggered() {
        return isTriggered;
    }

    public void setTriggered(Boolean triggered) {
        isTriggered = triggered;
    }
}
