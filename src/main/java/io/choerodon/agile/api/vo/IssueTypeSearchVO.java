package io.choerodon.agile.api.vo;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnore;
import io.swagger.annotations.ApiModelProperty;

import org.hzero.starter.keyencrypt.core.Encrypt;

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
    @ApiModelProperty(value = "是否启用")
    private Boolean enabled;
    @ApiModelProperty(value = "是否引用")
    private Boolean referenced;
    @ApiModelProperty(value = "来源")
    private String source;
    @JsonIgnore
    private List<String> typeCodes;
    @JsonIgnore
    private List<Long> issueTypeIds;

    @ApiModelProperty(value = "组织ID")
    private Long organizationId;

    @ApiModelProperty(value = "项目ID")
    private Long projectId;

    @Encrypt
    @ApiModelProperty(value = "过滤的问题类型id")
    private List<Long> filterIssueTypeIds;
    @ApiModelProperty(value = "排除的问题类型Code")
    private List<String> excludeTypeCodes;
    @ApiModelProperty(value = "项目id集合")
    private List<Long> projectIds;

    public List<Long> getIssueTypeIds() {
        return issueTypeIds;
    }

    public IssueTypeSearchVO setIssueTypeIds(List<Long> issueTypeIds) {
        this.issueTypeIds = issueTypeIds;
        return this;
    }

    public List<String> getTypeCodes() {
        return typeCodes;
    }

    public IssueTypeSearchVO setTypeCodes(List<String> typeCodes) {
        this.typeCodes = typeCodes;
        return this;
    }

    public Boolean getEnabled() {
        return enabled;
    }

    public IssueTypeSearchVO setEnabled(Boolean enabled) {
        this.enabled = enabled;
        return this;
    }

    public Boolean getReferenced() {
        return referenced;
    }

    public IssueTypeSearchVO setReferenced(Boolean referenced) {
        this.referenced = referenced;
        return this;
    }

    public String getSource() {
        return source;
    }

    public IssueTypeSearchVO setSource(String source) {
        this.source = source;
        return this;
    }

    public Long getOrganizationId() {
        return organizationId;
    }

    public IssueTypeSearchVO setOrganizationId(Long organizationId) {
        this.organizationId = organizationId;
        return this;
    }

    public Long getProjectId() {
        return projectId;
    }

    public IssueTypeSearchVO setProjectId(Long projectId) {
        this.projectId = projectId;
        return this;
    }

    public String getName() {
        return name;
    }

    public IssueTypeSearchVO setName(String name) {
        this.name = name;
        return this;
    }

    public String getDescription() {
        return description;
    }

    public IssueTypeSearchVO setDescription(String description) {
        this.description = description;
        return this;
    }

    public IssueTypeSearchVO setParam(String param) {
        this.param = param;
        return this;
    }

    public String getParam() {
        return param;
    }

    public List<Long> getFilterIssueTypeIds() {
        return filterIssueTypeIds;
    }

    public IssueTypeSearchVO setFilterIssueTypeIds(List<Long> filterIssueTypeIds) {
        this.filterIssueTypeIds = filterIssueTypeIds;
        return this;
    }

    /**
     * @return 排除的问题类型Code
     */
    public List<String> getExcludeTypeCodes() {
        return excludeTypeCodes;
    }

    public IssueTypeSearchVO setExcludeTypeCodes(List<String> excludeTypeCodes) {
        this.excludeTypeCodes = excludeTypeCodes;
        return this;
    }

    public List<Long> getProjectIds() {
        return projectIds;
    }

    public IssueTypeSearchVO setProjectIds(List<Long> projectIds) {
        this.projectIds = projectIds;
        return this;
    }
}
