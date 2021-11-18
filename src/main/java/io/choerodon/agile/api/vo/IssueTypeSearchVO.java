package io.choerodon.agile.api.vo;

import com.fasterxml.jackson.annotation.JsonIgnore;
import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

import java.util.List;

/**
 * Created by HuangFuqiang@choerodon.io on 2018/11/29.
 * Email: fuqianghuang01@gmail.com
 */
public class IssueTypeSearchVO {
    @ApiModelProperty(value = "查询名称")
    private String name;
    @ApiModelProperty(value = "查询描述")
    private String description;
    @ApiModelProperty(value = "其他参数")
    private String param;
    private Boolean enabled;

    private Boolean referenced;

    private String source;
    @JsonIgnore
    private List<String> typeCodes;
    @JsonIgnore
    private List<Long> issueTypeIds;

    @Encrypt
    private List<Long> filterIssueTypeIds;

    private List<Long> projectIds;

    public List<Long> getIssueTypeIds() {
        return issueTypeIds;
    }

    public void setIssueTypeIds(List<Long> issueTypeIds) {
        this.issueTypeIds = issueTypeIds;
    }

    public List<String> getTypeCodes() {
        return typeCodes;
    }

    public void setTypeCodes(List<String> typeCodes) {
        this.typeCodes = typeCodes;
    }

    public Boolean getEnabled() {
        return enabled;
    }

    public void setEnabled(Boolean enabled) {
        this.enabled = enabled;
    }

    public Boolean getReferenced() {
        return referenced;
    }

    public void setReferenced(Boolean referenced) {
        this.referenced = referenced;
    }

    public String getSource() {
        return source;
    }

    public void setSource(String source) {
        this.source = source;
    }

    private Long organizationId;

    private Long projectId;

    public Long getOrganizationId() {
        return organizationId;
    }

    public void setOrganizationId(Long organizationId) {
        this.organizationId = organizationId;
    }

    public Long getProjectId() {
        return projectId;
    }

    public void setProjectId(Long projectId) {
        this.projectId = projectId;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public void setParam(String param) {
        this.param = param;
    }

    public String getParam() {
        return param;
    }

    public List<Long> getFilterIssueTypeIds() {
        return filterIssueTypeIds;
    }

    public void setFilterIssueTypeIds(List<Long> filterIssueTypeIds) {
        this.filterIssueTypeIds = filterIssueTypeIds;
    }

    public List<Long> getProjectIds() {
        return projectIds;
    }

    public void setProjectIds(List<Long> projectIds) {
        this.projectIds = projectIds;
    }
}
