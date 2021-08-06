package io.choerodon.agile.api.vo.business;

import org.hzero.starter.keyencrypt.core.Encrypt;

import java.util.Set;

/**
 * @author superlee
 * @since 2021-08-02
 */
public class ConfigurationRuleSettingVO {
    @Encrypt
    private Long id;

    private Long ruleId;

    private String target;

    @Encrypt
    private Set<Long> linkTypeIds;

    private String action;

    private Long projectId;

    private Long organizationId;

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public Long getRuleId() {
        return ruleId;
    }

    public void setRuleId(Long ruleId) {
        this.ruleId = ruleId;
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

    public String getTarget() {
        return target;
    }

    public void setTarget(String target) {
        this.target = target;
    }

    public Set<Long> getLinkTypeIds() {
        return linkTypeIds;
    }

    public void setLinkTypeIds(Set<Long> linkTypeIds) {
        this.linkTypeIds = linkTypeIds;
    }

    public String getAction() {
        return action;
    }

    public void setAction(String action) {
        this.action = action;
    }
}
