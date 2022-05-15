package io.choerodon.agile.api.vo.business;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonInclude;
import io.choerodon.agile.api.vo.RuleExpressVO;
import io.choerodon.mybatis.domain.AuditDomain;
import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

/**
 * @author jiaxu.cui@hand-china.com 2020/9/23 上午9:50
 */
@JsonInclude(JsonInclude.Include.NON_NULL)
public class ConfigurationRuleVO extends AuditDomain {

    @Encrypt
    @ApiModelProperty(value = "主键")
    private Long id;
    @ApiModelProperty(value = "项目id")
    private Long projectId;
    @ApiModelProperty(value = "sql语句")
    private String sqlQuery;
    @ApiModelProperty(value = "表达式")
    private String expressQuery;
    @ApiModelProperty(value = "表达式格式")
    private String expressFormat;
    @ApiModelProperty(value = "名称")
    private String name;
    @ApiModelProperty(value = "启停用")
    private Boolean enabled;
    @ApiModelProperty(value = "来源")
    private String source;
    @ApiModelProperty("快速搜索创建传值")
    private List<RuleExpressVO> expressList;
    @ApiModelProperty(value = "序列")
    private Integer sequence;
    @Encrypt
    @ApiModelProperty(value = "问题类型id集合")
    private List<Long> issueTypeIds;
    @ApiModelProperty(value = "问题类型id")
    @Encrypt
    private Long issueTypeId;
    @ApiModelProperty(value = "问题类型名称")
    private String issueTypeNames;
    @ApiModelProperty(value = "规则评论配置")
    private List<ConfigurationRuleCommentVO> configurationRuleComments;
    @ApiModelProperty(value = "规则字段更新配置")
    private List<ConfigurationRuleUpdateIssueVO> configurationRuleUpdateIssue;
    @ApiModelProperty(value = "规则通知配置")
    private List<ConfigurationRuleNoticeVO> configurationRuleNotices;
    @ApiModelProperty(value = "是否允许被其他触发器触发")
    private Boolean triggered;

    public List<ConfigurationRuleCommentVO> getConfigurationRuleComments() {
        return configurationRuleComments;
    }

    public void setConfigurationRuleComments(List<ConfigurationRuleCommentVO> configurationRuleComments) {
        this.configurationRuleComments = configurationRuleComments;
    }

    public List<ConfigurationRuleNoticeVO> getConfigurationRuleNotices() {
        return configurationRuleNotices;
    }

    public void setConfigurationRuleNotices(List<ConfigurationRuleNoticeVO> configurationRuleNotices) {
        this.configurationRuleNotices = configurationRuleNotices;
    }

    public List<ConfigurationRuleUpdateIssueVO> getConfigurationRuleUpdateIssue() {
        return configurationRuleUpdateIssue;
    }

    public void setConfigurationRuleUpdateIssue(List<ConfigurationRuleUpdateIssueVO> configurationRuleUpdateIssue) {
        this.configurationRuleUpdateIssue = configurationRuleUpdateIssue;
    }

    public Boolean getTriggered() {
        return triggered;
    }

    public void setTriggered(Boolean triggered) {
        this.triggered = triggered;
    }

    public Long getIssueTypeId() {
        return issueTypeId;
    }

    public void setIssueTypeId(Long issueTypeId) {
        this.issueTypeId = issueTypeId;
    }

    public String getIssueTypeNames() {
        return issueTypeNames;
    }

    public void setIssueTypeNames(String issueTypeNames) {
        this.issueTypeNames = issueTypeNames;
    }

    public List<Long> getIssueTypeIds() {
        return issueTypeIds;
    }

    public void setIssueTypeIds(List<Long> issueTypeIds) {
        this.issueTypeIds = issueTypeIds;
    }

    public Integer getSequence() {
        return sequence;
    }

    public void setSequence(Integer sequence) {
        this.sequence = sequence;
    }

    public String getSource() {
        return source;
    }

    public void setSource(String source) {
        this.source = source;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public Boolean getEnabled() {
        return enabled;
    }

    public void setEnabled(Boolean enabled) {
        this.enabled = enabled;
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
