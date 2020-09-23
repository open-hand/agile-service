package io.choerodon.agile.api.vo;

import java.util.List;

import io.choerodon.mybatis.domain.AuditDomain;
import io.swagger.annotations.ApiModelProperty;

/**
 * @author jiaxu.cui@hand-china.com 2020/9/23 上午9:50
 */
public class ConfigurationRuleVO extends AuditDomain {

    private Long id;
    private Long projectId;
    private String name;
    private String sqlQuery;
    private String expressQuery;
    private String expressFormat;
    @ApiModelProperty(value = "快速搜索创建传值")
    private List<RuleExpressVO> expressList;

    public List<RuleExpressVO> getExpressList() {
        return expressList;
    }

    public void setExpressList(List<RuleExpressVO> expressList) {
        this.expressList = expressList;
    }

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

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
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
