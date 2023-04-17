package io.choerodon.agile.infra.dto;

import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.Table;

import io.choerodon.mybatis.annotation.ModifyAudit;
import io.choerodon.mybatis.annotation.VersionAudit;
import io.choerodon.mybatis.domain.AuditDomain;

/**
 * @author superlee
 * @since 2021-10-27
 */
@Table(name = "agile_issue_personal_sort")
@ModifyAudit
@VersionAudit
public class IssuePersonalSortDTO extends AuditDomain {

    public static final String FIELD_ID = "id";
    public static final String FIELD_USER_ID = "userId";
    public static final String FIELD_SORT_JSON = "sortJson";
    public static final String FIELD_PROJECT_ID = "projectId";
    public static final String FIELD_ORGANIZATION_ID = "organizationId";
    public static final String FIELD_BUSINESS_TYPE = "businessType";

    @Id
    @GeneratedValue
    private Long id;

    private Long userId;

    private String sortJson;

    private Long projectId;

    private Long organizationId;

    private String businessType;

    public String getBusinessType() {
        return businessType;
    }

    public void setBusinessType(String businessType) {
        this.businessType = businessType;
    }

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public Long getUserId() {
        return userId;
    }

    public void setUserId(Long userId) {
        this.userId = userId;
    }

    public String getSortJson() {
        return sortJson;
    }

    public void setSortJson(String sortJson) {
        this.sortJson = sortJson;
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
}
