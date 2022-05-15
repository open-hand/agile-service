package io.choerodon.agile.api.vo;

import io.swagger.annotations.ApiModelProperty;

import java.util.Date;

/**
 * @author: 25499
 * @date: 2019/11/25 18:32
 * @description:
 */
public class DataLogFixVO {

    @ApiModelProperty("日志id")
    private Long logId;
    @ApiModelProperty("项目id")
    private Long projectId;
    @ApiModelProperty("字段")
    private String field;
    @ApiModelProperty("旧值")
    private String oldValue;
    @ApiModelProperty("旧值字符串")
    private String oldString;
    @ApiModelProperty("新值")
    private String newValue;
    @ApiModelProperty("新值字符串")
    private String newString;
    @ApiModelProperty("问题id")
    private Long issueId;
    @ApiModelProperty("乐观锁")
    private Long objectVersionNumber;
    @ApiModelProperty("创建人")
    private Long createdBy;
    @ApiModelProperty("创建时间")
    private Date creationDate;
    @ApiModelProperty("更新人")
    private Long lastUpdatedBy;
    @ApiModelProperty("更新时间")
    private Date lastUpdateDate;

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

    public String getField() {
        return field;
    }

    public void setField(String field) {
        this.field = field;
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

    public Long getIssueId() {
        return issueId;
    }

    public void setIssueId(Long issueId) {
        this.issueId = issueId;
    }

    public Long getObjectVersionNumber() {
        return objectVersionNumber;
    }

    public void setObjectVersionNumber(Long objectVersionNumber) {
        this.objectVersionNumber = objectVersionNumber;
    }

    public Long getCreatedBy() {
        return createdBy;
    }

    public void setCreatedBy(Long createdBy) {
        this.createdBy = createdBy;
    }

    public Date getCreationDate() {
        return creationDate;
    }

    public void setCreationDate(Date creationDate) {
        this.creationDate = creationDate;
    }

    public Long getLastUpdatedBy() {
        return lastUpdatedBy;
    }

    public void setLastUpdatedBy(Long lastUpdatedBy) {
        this.lastUpdatedBy = lastUpdatedBy;
    }

    public Date getLastUpdateDate() {
        return lastUpdateDate;
    }

    public void setLastUpdateDate(Date lastUpdateDate) {
        this.lastUpdateDate = lastUpdateDate;
    }
}
