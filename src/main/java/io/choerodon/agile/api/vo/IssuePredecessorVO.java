package io.choerodon.agile.api.vo;

import io.choerodon.agile.api.vo.business.IssueListVO;
import org.hzero.starter.keyencrypt.core.Encrypt;

/**
 * @author superlee
 * @since 2021-11-10
 */
public class IssuePredecessorVO {

    @Encrypt
    private Long id;

    @Encrypt
    private Long issueId;

    @Encrypt
    private Long predecessorId;

    private String predecessorType;

    private Long projectId;

    private Long organizationId;

    private IssueListVO predecessorIssueVO;

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public Long getIssueId() {
        return issueId;
    }

    public void setIssueId(Long issueId) {
        this.issueId = issueId;
    }

    public Long getPredecessorId() {
        return predecessorId;
    }

    public void setPredecessorId(Long predecessorId) {
        this.predecessorId = predecessorId;
    }

    public String getPredecessorType() {
        return predecessorType;
    }

    public void setPredecessorType(String predecessorType) {
        this.predecessorType = predecessorType;
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

    public IssueListVO getPredecessorIssueVO() {
        return predecessorIssueVO;
    }

    public void setPredecessorIssueVO(IssueListVO predecessorIssueVO) {
        this.predecessorIssueVO = predecessorIssueVO;
    }
}
