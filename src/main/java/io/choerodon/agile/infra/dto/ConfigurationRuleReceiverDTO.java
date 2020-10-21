package io.choerodon.agile.infra.dto;

import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.Table;

import io.choerodon.mybatis.annotation.ModifyAudit;
import io.choerodon.mybatis.annotation.VersionAudit;
import io.choerodon.mybatis.domain.AuditDomain;
import io.swagger.annotations.ApiModel;

/**
 * @author jiaxu.cui@hand-china.com 2020-09-23 09:29:15
 */
@ApiModel("页面规则接收人")
@VersionAudit
@ModifyAudit
@Table(name = "agile_configuration_rule_receiver")
public class ConfigurationRuleReceiverDTO extends AuditDomain {

    public static final String TYPE_RECEIVER = "RECEIVER";
    public static final String TYPE_CC = "CC";
    public static final String TYPE_ASSIGNEE = "ASSIGNEE";

    public ConfigurationRuleReceiverDTO(Long ruleId, Long projectId) {
        this.ruleId = ruleId;
        this.projectId = projectId;
    }

    public ConfigurationRuleReceiverDTO() {
    }

    @Id
    @GeneratedValue
    private Long id;
    private Long ruleId;
    private Long userId;
    private String userType;
    private Long projectId;

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

    public Long getUserId() {
        return userId;
    }

    public void setUserId(Long userId) {
        this.userId = userId;
    }

    public String getUserType() {
        return userType;
    }

    public void setUserType(String userType) {
        this.userType = userType;
    }

    public Long getProjectId() {
        return projectId;
    }

    public void setProjectId(Long projectId) {
        this.projectId = projectId;
    }
}
