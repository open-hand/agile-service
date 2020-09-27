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
@ApiModel("页面规则")
@VersionAudit
@ModifyAudit
@Table(name = "agile_configuration_rule")
public class ConfigurationRuleDTO extends AuditDomain {
    
    public static final String FIELD_SQL_QUERY = "sqlQuery";
    public static final String FIELD_EXPRESS_QUERY = "expressQuery";
    public static final String FIELD_EXPRESS_FORMAT = "expressFormat";

    public ConfigurationRuleDTO(Long id, Long projectId) {
        this.id = id;
        this.projectId = projectId;
    }

    public ConfigurationRuleDTO() {
    }

    @Id
    @GeneratedValue
    private Long id;
    private Long projectId;
    private String sqlQuery;
    private String expressQuery;
    private String expressFormat;

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public Long getProjectId() {
        return projectId;
    }

    public void setProjectId(Long projectId) {
        this.projectId = projectId;
    }

    public String getSqlQuery() {
        return sqlQuery;
    }

    public void setSqlQuery(String sqlQuery) {
        this.sqlQuery = sqlQuery;
    }

    public String getExpressQuery() {
        return expressQuery;
    }

    public void setExpressQuery(String expressQuery) {
        this.expressQuery = expressQuery;
    }

    public String getExpressFormat() {
        return expressFormat;
    }

    public void setExpressFormat(String expressFormat) {
        this.expressFormat = expressFormat;
    }
}
