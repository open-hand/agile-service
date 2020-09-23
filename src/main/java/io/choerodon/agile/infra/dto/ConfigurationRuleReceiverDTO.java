package io.choerodon.agile.infra.dto;

import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.Table;
import javax.validation.constraints.NotNull;

import org.hibernate.validator.constraints.NotBlank;
import io.choerodon.mybatis.domain.AuditDomain;
import io.choerodon.mybatis.annotation.ModifyAudit;
import io.choerodon.mybatis.annotation.VersionAudit;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;

/**
 * 
 *
 * @author jiaxu.cui@hand-china.com 2020-09-23 09:29:15
 */
@ApiModel("页面规则接收人")
@VersionAudit
@ModifyAudit
@Table(name = "agile_configuration_rule_receiver")
public class ConfigurationRuleReceiverDTO extends AuditDomain {

    public static final String FIELD_ID = "id";
    public static final String FIELD_RULE_ID = "ruleId";
    public static final String FIELD_USER_ID = "userId";
    public static final String FIELD_USER_TYPE = "userType";
    public static final String FIELD_PROJECT_ID = "projectId";

//
// 业务方法(按public protected private顺序排列)
// ------------------------------------------------------------------------------

//
// 数据库字段
// ------------------------------------------------------------------------------

    @Id
    @GeneratedValue
    private Long id;
    private Long ruleId;
    private Long userId;
    private String userType;
    private Long projectId;

//
// 非数据库字段
// ------------------------------------------------------------------------------

//
// getter/setter
// ------------------------------------------------------------------------------


    /**
     * @return 
     */
    public Long getId() {
            return id;
    }

    public void setId(Long id) {
            this.id = id;
    }
    /**
     * @return rule id
     */
    public Long getRuleId() {
            return ruleId;
    }

    public void setRuleId(Long ruleId) {
            this.ruleId = ruleId;
    }
    /**
     * @return rule id
     */
    public Long getUserId() {
            return userId;
    }

    public void setUserId(Long userId) {
            this.userId = userId;
    }
    /**
     * @return user type
     */
    public String getUserType() {
            return userType;
    }

    public void setUserType(String userType) {
            this.userType = userType;
    }
    /**
     * @return project id
     */
    public Long getProjectId() {
            return projectId;
    }

    public void setProjectId(Long projectId) {
            this.projectId = projectId;
    }
}
