package io.choerodon.agile.infra.dto;

import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.Table;

import io.choerodon.mybatis.annotation.ModifyAudit;
import io.choerodon.mybatis.annotation.VersionAudit;
import io.choerodon.mybatis.domain.AuditDomain;

/**
 * @author shinan.chen
 * @since 2019/2/25
 */
@Table(name = "agile_personal_filter")
@ModifyAudit
@VersionAudit
public class PersonalFilterDTO extends AuditDomain {

    public static final String FIELD_ORGANIZATION_ID = "organizationId";
    public static final String FIELD_PROJECT_ID = "projectId";
    public static final String FIELD_USER_ID = "userId";
    public static final String FIELD_NAME = "name";
    public static final String FIELD_FILTER_TYPE_CODE = "filterTypeCode";
    public static final String FIELD_FILTER_ID = "filterId";

    @Id
    @GeneratedValue
    private Long filterId;
    private Long projectId;
    private Long userId;
    private String name;
    private String filterJson;
    private Boolean isDefault;
    private String filterTypeCode;
    private Long organizationId;

    public Long getFilterId() {
        return filterId;
    }

    public void setFilterId(Long filterId) {
        this.filterId = filterId;
    }

    public Long getProjectId() {
        return projectId;
    }

    public void setProjectId(Long projectId) {
        this.projectId = projectId;
    }

    public Long getUserId() {
        return userId;
    }

    public void setUserId(Long userId) {
        this.userId = userId;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getFilterJson() {
        return filterJson;
    }

    public void setFilterJson(String filterJson) {
        this.filterJson = filterJson;
    }

    public Boolean getDefault() {
        return isDefault;
    }

    public void setDefault(Boolean aDefault) {
        isDefault = aDefault;
    }

    public String getFilterTypeCode() {
        return filterTypeCode;
    }

    public void setFilterTypeCode(String filterTypeCode) {
        this.filterTypeCode = filterTypeCode;
    }

    public Long getOrganizationId() {
        return organizationId;
    }

    public void setOrganizationId(Long organizationId) {
        this.organizationId = organizationId;
    }
}

