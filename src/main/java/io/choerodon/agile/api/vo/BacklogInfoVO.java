package io.choerodon.agile.api.vo;

import java.util.Date;
import java.util.List;

import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

public class BacklogInfoVO {
    @Encrypt
    @ApiModelProperty("需求id")
    private Long id;
    @ApiModelProperty("组织id")
    private Long organizationId;
    @ApiModelProperty("项目id")
    private Long projectId;
    @ApiModelProperty("状态id")
    private Long statusId;
    @ApiModelProperty("需求编号")
    private String backlogNum;
    @ApiModelProperty("概要")
    private String summary;
    @ApiModelProperty("需求类型")
    private String backlogTypeName;
    @ApiModelProperty("需求类型")
    private String backlogClassificationName;
    @ApiModelProperty("处理人id集合")
    private List<Long> assigneeIds;
    @ApiModelProperty("创建人")
    private Long createdBy;
    @ApiModelProperty("更新人")
    private Long lastUpdatedBy;
    @ApiModelProperty(value = "创建时间")
    private Date creationDate;
    @ApiModelProperty(value = "最后更新时间")
    private Date lastUpdateDate;

    public BacklogInfoVO() {
    }

    public BacklogInfoVO(Long id, Long organizationId, Long projectId) {
        this.id = id;
        this.organizationId = organizationId;
        this.projectId = projectId;
    }

    public BacklogInfoVO(Long id, Long organizationId, Long projectId, Long statusId) {
        this.id = id;
        this.organizationId = organizationId;
        this.projectId = projectId;
        this.statusId = statusId;
    }

    public Long getStatusId() {
        return statusId;
    }

    public void setStatusId(Long statusId) {
        this.statusId = statusId;
    }

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public Long getOrganizationId() {
        return organizationId;
    }

    public void setOrganizationId(Long organizationId) {
        this.organizationId = organizationId;
    }

    public Long getProjectId() {
        return projectId;
    }

    public void setProjectId(Long projectId) {
        this.projectId = projectId;
    }

    public String getBacklogNum() {
        return backlogNum;
    }

    public void setBacklogNum(String backlogNum) {
        this.backlogNum = backlogNum;
    }

    public String getSummary() {
        return summary;
    }

    public void setSummary(String summary) {
        this.summary = summary;
    }

    public List<Long> getAssigneeIds() {
        return assigneeIds;
    }

    public void setAssigneeIds(List<Long> assigneeIds) {
        this.assigneeIds = assigneeIds;
    }

    public Long getCreatedBy() {
        return createdBy;
    }

    public void setCreatedBy(Long createdBy) {
        this.createdBy = createdBy;
    }

    public Long getLastUpdatedBy() {
        return lastUpdatedBy;
    }

    public void setLastUpdatedBy(Long lastUpdatedBy) {
        this.lastUpdatedBy = lastUpdatedBy;
    }

    public Date getCreationDate() {
        return creationDate;
    }

    public void setCreationDate(Date creationDate) {
        this.creationDate = creationDate;
    }

    public Date getLastUpdateDate() {
        return lastUpdateDate;
    }

    public void setLastUpdateDate(Date lastUpdateDate) {
        this.lastUpdateDate = lastUpdateDate;
    }

    public String getBacklogTypeName() {
        return backlogTypeName;
    }

    public void setBacklogTypeName(String backlogTypeName) {
        this.backlogTypeName = backlogTypeName;
    }

    public String getBacklogClassificationName() {
        return backlogClassificationName;
    }

    public void setBacklogClassificationName(String backlogClassificationName) {
        this.backlogClassificationName = backlogClassificationName;
    }
}
