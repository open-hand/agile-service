package io.choerodon.agile.api.vo.business;

import io.swagger.annotations.ApiModelProperty;

import java.util.Set;

/**
 * @author superlee
 * @since 2020-12-17
 */
public class RuleLogRelVO {
    @ApiModelProperty(value = "主键")
    private Long id;
    @ApiModelProperty(value = "日志id")
    private Long logId;
    @ApiModelProperty(value = "规则id")
    private Long ruleId;
    @ApiModelProperty(value = "业务类型")
    private String businessType;
    @ApiModelProperty(value = "实例id")
    private Long instanceId;
    @ApiModelProperty(value = "项目id")
    private Long projectId;
    @ApiModelProperty(value = "组织id")
    private Long organizationId;
    @ApiModelProperty(value = "规则名称")
    private String ruleName;
    @ApiModelProperty(value = "搜索日志id集合")
    private Set<Long> searchLogIds;

    public RuleLogRelVO() {}

    public RuleLogRelVO(Long logId,
                        Long ruleId,
                        String businessType,
                        Long instanceId,
                        Long projectId,
                        Long organizationId) {
        this.logId = logId;
        this.ruleId = ruleId;
        this.businessType = businessType;
        this.instanceId = instanceId;
        this.projectId = projectId;
        this.organizationId = organizationId;
    }

    public Set<Long> getSearchLogIds() {
        return searchLogIds;
    }

    public void setSearchLogIds(Set<Long> searchLogIds) {
        this.searchLogIds = searchLogIds;
    }

    public String getRuleName() {
        return ruleName;
    }

    public void setRuleName(String ruleName) {
        this.ruleName = ruleName;
    }

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public Long getLogId() {
        return logId;
    }

    public void setLogId(Long logId) {
        this.logId = logId;
    }

    public Long getRuleId() {
        return ruleId;
    }

    public void setRuleId(Long ruleId) {
        this.ruleId = ruleId;
    }

    public String getBusinessType() {
        return businessType;
    }

    public void setBusinessType(String businessType) {
        this.businessType = businessType;
    }

    public Long getInstanceId() {
        return instanceId;
    }

    public void setInstanceId(Long instanceId) {
        this.instanceId = instanceId;
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
