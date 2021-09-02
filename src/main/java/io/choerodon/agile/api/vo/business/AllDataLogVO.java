package io.choerodon.agile.api.vo.business;

import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

import java.util.Date;

import io.choerodon.agile.api.vo.IssueTypeVO;
import io.choerodon.agile.api.vo.StatusVO;
import io.choerodon.agile.infra.dto.UserMessageDTO;

/**
 * @author chihao.ran@hand-china.com
 * @since 2021-03-23
 */

public class AllDataLogVO {
    @ApiModelProperty(value = "日志主键id")
    @Encrypt
    private Long logId;
    @ApiModelProperty(value = "项目id")
    private Long projectId;
    @ApiModelProperty(value = "字段")
    private String field;
    @ApiModelProperty(value = "字段名称")
    private String fieldName;
    @ApiModelProperty(value = "日志旧值")
    private String oldValue;
    @ApiModelProperty(value = "日志旧值")
    private String oldString;
    @ApiModelProperty(value = "日志新值")
    private String newValue;
    @ApiModelProperty(value = "日志新值")
    private String newString;
    @ApiModelProperty(value = "instanceId")
    @Encrypt
    private Long instanceId;
    @ApiModelProperty(value = "创建日期")
    private Date creationDate;
    @ApiModelProperty(value = "创建用户id")
    @Encrypt
    private Long createdBy;
    @ApiModelProperty(value = "创建用户")
    private UserMessageDTO createdByUser;
    @ApiModelProperty(value = "是否自定义字段日志")
    private Boolean isCusLog;
    @ApiModelProperty(value = "日志类型")
    private String logType;
    @ApiModelProperty(value = "问题类型")
    private IssueTypeVO issueTypeVO;
    @ApiModelProperty(value = "num")
    private String num;
    @ApiModelProperty(value = "summary")
    private String summary;
    @ApiModelProperty(value = "项目名称")
    private String projectName;
    @ApiModelProperty(value = "状态")
    private StatusVO statusVO;
    @ApiModelProperty(value = "规则名称")
    private String ruleName;

    public Boolean getCusLog() {
        return isCusLog;
    }

    public void setCusLog(Boolean cusLog) {
        isCusLog = cusLog;
    }

    public String getRuleName() {
        return ruleName;
    }

    public void setRuleName(String ruleName) {
        this.ruleName = ruleName;
    }

    public Date getCreationDate() {
        return creationDate;
    }

    public void setCreationDate(Date creationDate) {
        this.creationDate = creationDate;
    }

    public Long getCreatedBy() {
        return createdBy;
    }

    public void setCreatedBy(Long createdBy) {
        this.createdBy = createdBy;
    }

    public Boolean getIsCusLog() {
        return isCusLog;
    }

    public void setIsCusLog(Boolean cusLog) {
        isCusLog = cusLog;
    }

    public String getFieldName() {
        return fieldName;
    }

    public void setFieldName(String fieldName) {
        this.fieldName = fieldName;
    }

    public Long getLogId() {
        return logId;
    }

    public void setLogId(Long logId) {
        this.logId = logId;
    }

    public Long getProjectId() {
        return projectId;
    }

    public void setProjectId(Long projectId) {
        this.projectId = projectId;
    }

    public void setField(String field) {
        this.field = field;
    }

    public String getField() {
        return field;
    }

    public String getOldValue() {
        return oldValue;
    }

    public void setOldValue(String oldValue) {
        this.oldValue = oldValue;
    }

    public String getOldString() {
        return oldString;
    }

    public void setOldString(String oldString) {
        this.oldString = oldString;
    }

    public String getNewValue() {
        return newValue;
    }

    public void setNewValue(String newValue) {
        this.newValue = newValue;
    }

    public String getNewString() {
        return newString;
    }

    public void setNewString(String newString) {
        this.newString = newString;
    }

    public Long getInstanceId() {
        return instanceId;
    }

    public void setInstanceId(Long instanceId) {
        this.instanceId = instanceId;
    }

    public UserMessageDTO getCreatedByUser() {
        return createdByUser;
    }

    public void setCreatedByUser(UserMessageDTO createdByUser) {
        this.createdByUser = createdByUser;
    }

    public String getLogType() {
        return logType;
    }

    public void setLogType(String logType) {
        this.logType = logType;
    }

    public IssueTypeVO getIssueTypeVO() {
        return issueTypeVO;
    }

    public void setIssueTypeVO(IssueTypeVO issueTypeVO) {
        this.issueTypeVO = issueTypeVO;
    }

    public String getNum() {
        return num;
    }

    public void setNum(String num) {
        this.num = num;
    }

    public String getSummary() {
        return summary;
    }

    public void setSummary(String summary) {
        this.summary = summary;
    }

    public String getProjectName() {
        return projectName;
    }

    public void setProjectName(String projectName) {
        this.projectName = projectName;
    }

    public StatusVO getStatusVO() {
        return statusVO;
    }

    public void setStatusVO(StatusVO statusVO) {
        this.statusVO = statusVO;
    }
}
