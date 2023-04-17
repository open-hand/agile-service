package io.choerodon.agile.infra.dto;

import java.util.Date;
import java.util.List;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.Table;
import javax.persistence.Transient;
import javax.validation.constraints.NotNull;

import io.choerodon.agile.app.service.ProductVersionService;
import io.choerodon.agile.infra.utils.StringUtil;
import io.choerodon.core.oauth.DetailsHelper;
import io.choerodon.mybatis.annotation.ModifyAudit;
import io.choerodon.mybatis.annotation.VersionAudit;
import io.choerodon.mybatis.domain.AuditDomain;

/**
 * @author dinghuang123@gmail.com
 * @since 2018-05-15 16:21:18
 */
@Table(name = "agile_version_issue_rel")
@ModifyAudit
@VersionAudit
public class VersionIssueRelDTO extends AuditDomain{

    @Id
    @GeneratedValue
    private Long id;  //需要加主键id

    /**
     * version id
     */
    @NotNull(message = "error.version_issue_rel.version_idNotNull")
    private Long versionId;

    /**
     * issue id
     */
    @NotNull(message = "error.version_issue_rel.issue_idNotNull")
    private Long issueId;

    @Transient
    private String name;

    @Transient
    private String statusCode;

    private String relationType;

    private Long projectId;

    private Date creationDate;

    private Long createdBy;

    @Transient
    private List<Long> issueIds;

    public VersionIssueRelDTO setId(Long id) {
        this.id = id;
        return this;
    }

    public Long getId() {
        return id;
    }

    public Long getVersionId() {
        return versionId;
    }

    public VersionIssueRelDTO setVersionId(Long versionId) {
        this.versionId = versionId;
        return this;
    }

    public Long getIssueId() {
        return issueId;
    }

    public VersionIssueRelDTO setIssueId(Long issueId) {
        this.issueId = issueId;
        return this;
    }

    public String getName() {
        return name;
    }

    public VersionIssueRelDTO setName(String name) {
        this.name = name;
        return this;
    }

    public String getRelationType() {
        return relationType;
    }

    public VersionIssueRelDTO setRelationType(String relationType) {
        this.relationType = relationType;
        return this;
    }

    public Long getProjectId() {
        return projectId;
    }

    public VersionIssueRelDTO setProjectId(Long projectId) {
        this.projectId = projectId;
        return this;
    }

    public String getStatusCode() {
        return statusCode;
    }

    public VersionIssueRelDTO setStatusCode(String statusCode) {
        this.statusCode = statusCode;
        return this;
    }

    @Override
    public Date getCreationDate() {
        return creationDate;
    }

    @Override
    public void setCreationDate(Date creationDate) {
        this.creationDate = creationDate;
    }

    @Override
    public Long getCreatedBy() {
        return createdBy;
    }

    @Override
    public void setCreatedBy(Long createdBy) {
        this.createdBy = createdBy;
    }

    public List<Long> getIssueIds() {
        return issueIds;
    }

    public VersionIssueRelDTO setIssueIds(List<Long> issueIds) {
        this.issueIds = issueIds;
        return this;
    }

    @Override
    public String toString() {
        return StringUtil.getToString(this);
    }

    public ProductVersionDTO createProductVersionDTO() {
        ProductVersionDTO productVersionDTO = new ProductVersionDTO();
        productVersionDTO.setStatusCode(ProductVersionService.VERSION_STATUS_CODE_PLANNING);
        productVersionDTO.setName(this.name);
        productVersionDTO.setProjectId(this.projectId);
        return productVersionDTO;
    }

    public void createBatchDeleteVersionIssueRel(Long projectId, Long issueId, String relationType) {
        this.projectId = projectId;
        this.issueId = issueId;
        this.relationType = relationType;
    }

    public void createBatchIssueToVersionDTO(Long projectId, Long versionId, List<Long> issueIds) {
        this.projectId = projectId;
        this.versionId = versionId;
        this.issueIds = issueIds;
        this.creationDate = new Date();
        this.createdBy = DetailsHelper.getUserDetails().getUserId();
    }

}
