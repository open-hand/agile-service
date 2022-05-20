package io.choerodon.agile.api.vo;

import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

import javax.validation.constraints.NotNull;

/**
 * @author shinan.chen
 * @date 2018/8/8
 */
@JsonInclude(JsonInclude.Include.NON_NULL)
public class IssueTypeVO {
    @ApiModelProperty(value = "问题类型id")
    @Encrypt
    private Long id;
    @ApiModelProperty(value = "名称")
    @NotNull(message = "error.name.null")
    private String name;
    @ApiModelProperty(value = "图标")
    private String icon;
    @ApiModelProperty(value = "描述")
    private String description;
    @ApiModelProperty(value = "组织id")
    private Long organizationId;
    @ApiModelProperty(value = "颜色")
    private String colour;
    @ApiModelProperty(value = "类型编码")
    private String typeCode;
    @ApiModelProperty(value = "是否初始化")
    private Boolean initialize;
    @ApiModelProperty(value = "乐观锁")
    private Long objectVersionNumber;
    @ApiModelProperty(value = "引用问题类型")
    private IssueTypeVO referenceIssueType;

    /**
     * 用于做状态机方案的配置时
     */
    @ApiModelProperty(value = "状态机名称")
    private String stateMachineName;
    @ApiModelProperty(value = "状态机id")
    @Encrypt
    private Long stateMachineId;
    @ApiModelProperty(value = "项目id")
    private Long projectId;
    @ApiModelProperty(value = "是否启用")
    private Boolean enabled;
    @ApiModelProperty(value = "是否可以被引用")
    private Boolean referenced;
    @ApiModelProperty(value = "使用数量")
    private Integer usageCount;
    @ApiModelProperty(value = "来源")
    private String source;

    /**
     * 是否可以被删除
     */
    @ApiModelProperty(value = "是否可以被删除")
    private Boolean deleted;
    @ApiModelProperty(value = "引用id")
    private Long referenceId;
    @ApiModelProperty(value = "是否复制状态机")
    private Boolean copyStatusMachine;
    @ApiModelProperty(value = "是否复制自定义字段配置")
    private Boolean copyCustomField;
    @ApiModelProperty(value = "排序值")
    private String rank;

    public String getRank() {
        return rank;
    }

    public void setRank(String rank) {
        this.rank = rank;
    }

    public IssueTypeVO getReferenceIssueType() {
        return referenceIssueType;
    }

    public void setReferenceIssueType(IssueTypeVO referenceIssueType) {
        this.referenceIssueType = referenceIssueType;
    }

    public Long getReferenceId() {
        return referenceId;
    }

    public void setReferenceId(Long referenceId) {
        this.referenceId = referenceId;
    }

    public Boolean getDeleted() {
        return deleted;
    }

    public void setDeleted(Boolean deleted) {
        this.deleted = deleted;
    }

    public String getSource() {
        return source;
    }

    public void setSource(String source) {
        this.source = source;
    }

    public Integer getUsageCount() {
        return usageCount;
    }

    public void setUsageCount(Integer usageCount) {
        this.usageCount = usageCount;
    }

    public Long getProjectId() {
        return projectId;
    }

    public void setProjectId(Long projectId) {
        this.projectId = projectId;
    }

    public Boolean getEnabled() {
        return enabled;
    }

    public void setEnabled(Boolean enabled) {
        this.enabled = enabled;
    }

    public Boolean getReferenced() {
        return referenced;
    }

    public void setReferenced(Boolean referenced) {
        this.referenced = referenced;
    }

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public Long getOrganizationId() {
        return organizationId;
    }

    public void setOrganizationId(Long organizationId) {
        this.organizationId = organizationId;
    }

    public Long getObjectVersionNumber() {
        return objectVersionNumber;
    }

    public void setObjectVersionNumber(Long objectVersionNumber) {
        this.objectVersionNumber = objectVersionNumber;
    }

    public String getIcon() {
        return icon;
    }

    public void setIcon(String icon) {
        this.icon = icon;
    }

    public String getStateMachineName() {
        return stateMachineName;
    }

    public void setStateMachineName(String stateMachineName) {
        this.stateMachineName = stateMachineName;
    }

    public String getColour() {
        return colour;
    }

    public void setColour(String colour) {
        this.colour = colour;
    }

    public String getTypeCode() {
        return typeCode;
    }

    public void setTypeCode(String typeCode) {
        this.typeCode = typeCode;
    }

    public Boolean getInitialize() {
        return initialize;
    }

    public void setInitialize(Boolean initialize) {
        this.initialize = initialize;
    }

    public Long getStateMachineId() {
        return stateMachineId;
    }

    public void setStateMachineId(Long stateMachineId) {
        this.stateMachineId = stateMachineId;
    }

    public Boolean getCopyStatusMachine() {
        return copyStatusMachine;
    }

    public void setCopyStatusMachine(Boolean copyStatusMachine) {
        this.copyStatusMachine = copyStatusMachine;
    }

    public Boolean getCopyCustomField() {
        return copyCustomField;
    }

    public void setCopyCustomField(Boolean copyCustomField) {
        this.copyCustomField = copyCustomField;
    }

    @Override
    public String toString() {
        return "IssueTypeVO{" +
                "id=" + id +
                ", name='" + name + '\'' +
                ", icon='" + icon + '\'' +
                ", description='" + description + '\'' +
                ", organizationId=" + organizationId +
                ", colour='" + colour + '\'' +
                ", typeCode='" + typeCode + '\'' +
                ", initialize=" + initialize +
                ", objectVersionNumber=" + objectVersionNumber +
                ", stateMachineName='" + stateMachineName + '\'' +
                ", stateMachineId=" + stateMachineId +
                ", projectId=" + projectId +
                ", enabled=" + enabled +
                ", referenced=" + referenced +
                ", usageCount=" + usageCount +
                ", source='" + source + '\'' +
                '}';
    }
}
