package io.choerodon.agile.api.vo.business;

import java.util.Set;

/**
 * @author superlee
 * @since 2020-12-17
 */
public class RuleLogRelVO {

    private Long id;

    private Long logId;

    private Long ruleId;

    private String businessType;

    private Long instanceId;

    private Long projectId;

    private Long organizationId;

    private String ruleName;

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
