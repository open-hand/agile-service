package io.choerodon.agile.api.vo;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonInclude;
import io.choerodon.agile.infra.dto.UserDTO;
import io.choerodon.mybatis.domain.AuditDomain;
import io.swagger.annotations.ApiModelProperty;

/**
 * @author jiaxu.cui@hand-china.com 2020/9/23 上午9:50
 */
@JsonInclude(JsonInclude.Include.NON_NULL)
public class ConfigurationRuleVO extends AuditDomain {

    private Long id;
    private Long projectId;
    private String sqlQuery;
    private String expressQuery;
    private String expressFormat;
    @ApiModelProperty("快速搜索创建传值")
    private List<RuleExpressVO> expressList;
    @ApiModelProperty("通知对象")
    private List<UserDTO> receiverList;
    @ApiModelProperty("抄送人")
    private List<UserDTO> ccList;

    public List<UserDTO> getReceiverList() {
        return receiverList;
    }

    public void setReceiverList(List<UserDTO> receiverList) {
        this.receiverList = receiverList;
    }

    public List<UserDTO> getCcList() {
        return ccList;
    }

    public void setCcList(List<UserDTO> ccList) {
        this.ccList = ccList;
    }

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
