package io.choerodon.agile.api.vo;


import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

import java.util.List;

/**
 * @author shinan.chen
 * @since 2019/3/29
 */
public class ObjectSchemeFieldVO {
    @ApiModelProperty(value = "字段id")
    @Encrypt
    private Long id;
    @ApiModelProperty(value = "字段编码")
    private String code;
    @ApiModelProperty(value = "名称")
    private String name;
    @ApiModelProperty(value = "描述")
    private String description;
    @ApiModelProperty(value = "字段类型")
    private String fieldType;
    @ApiModelProperty(value = "字段类型名称")
    private String fieldTypeName;
    @ApiModelProperty(value = "是否系统")
    private Boolean system;
    @ApiModelProperty(value = "是否必填")
    private Boolean required;
    private String requiredScope;
    @ApiModelProperty(value = "上下文")
    private String context;
    @ApiModelProperty(value = "上下文名称")
    private String contextName;
    @ApiModelProperty(value = "对象方案编码")
    private String schemeCode;
    @ApiModelProperty(value = "默认值")
    private String defaultValue;
    @ApiModelProperty(value = "项目id")
    private Long projectId;
    @ApiModelProperty(value = "组织id")
    private Long organizationId;
    @ApiModelProperty(value = "乐观锁")
    private Long objectVersionNumber;
    @ApiModelProperty(value = "选项")
    private List<FieldOptionVO> fieldOptions;
    @ApiModelProperty(value = "默认值（用于显示，仅member类型）")
    private Object defaultValueObj;
    @ApiModelProperty(value = "数组化的context")
    private List<String> contexts;
    @ApiModelProperty(value = "是否允许小数")
    private Boolean extraConfig;
    @ApiModelProperty(value = "问题类型范围")
    private List<IssueTypeVO> issueTypeVOList;
    @ApiModelProperty(value = "问题类型id")
    @Encrypt
    private List<Long> issueTypeIds;

    public Boolean getExtraConfig() {
        return extraConfig;
    }

    public void setExtraConfig(Boolean extraConfig) {
        this.extraConfig = extraConfig;
    }

    public List<String> getContexts() {
        return contexts;
    }

    public void setContexts(List<String> contexts) {
        this.contexts = contexts;
    }

    public String getRequiredScope() {
        return requiredScope;
    }

    public void setRequiredScope(String requiredScope) {
        this.requiredScope = requiredScope;
    }

    public String getDefaultValue() {
        return defaultValue;
    }

    public void setDefaultValue(String defaultValue) {
        this.defaultValue = defaultValue;
    }

    public Long getObjectVersionNumber() {
        return objectVersionNumber;
    }

    public void setObjectVersionNumber(Long objectVersionNumber) {
        this.objectVersionNumber = objectVersionNumber;
    }

    public String getContextName() {
        return contextName;
    }

    public void setContextName(String contextName) {
        this.contextName = contextName;
    }

    public String getFieldTypeName() {
        return fieldTypeName;
    }

    public void setFieldTypeName(String fieldTypeName) {
        this.fieldTypeName = fieldTypeName;
    }

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public String getCode() {
        return code;
    }

    public void setCode(String code) {
        this.code = code;
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

    public String getFieldType() {
        return fieldType;
    }

    public void setFieldType(String fieldType) {
        this.fieldType = fieldType;
    }

    public Boolean getSystem() {
        return system;
    }

    public void setSystem(Boolean system) {
        this.system = system;
    }

    public Boolean getRequired() {
        return required;
    }

    public void setRequired(Boolean required) {
        this.required = required;
    }

    public String getContext() {
        return context;
    }

    public void setContext(String context) {
        this.context = context;
    }

    public String getSchemeCode() {
        return schemeCode;
    }

    public void setSchemeCode(String schemeCode) {
        this.schemeCode = schemeCode;
    }

    public Long getProjectId() {
        return projectId;
    }

    public void setProjectId(Long projectId) {
        this.projectId = projectId;
    }

    public Long getOrganizationId() {
        return organizationId;
    }

    public void setOrganizationId(Long organizationId) {
        this.organizationId = organizationId;
    }

    public List<FieldOptionVO> getFieldOptions() {
        return fieldOptions;
    }

    public void setFieldOptions(List<FieldOptionVO> fieldOptions) {
        this.fieldOptions = fieldOptions;
    }

    public Object getDefaultValueObj() {
        return defaultValueObj;
    }

    public void setDefaultValueObj(Object defaultValueObj) {
        this.defaultValueObj = defaultValueObj;
    }

    public List<IssueTypeVO> getIssueTypeVOList() {
        return issueTypeVOList;
    }

    public void setIssueTypeVOList(List<IssueTypeVO> issueTypeVOList) {
        this.issueTypeVOList = issueTypeVOList;
    }

    public List<Long> getIssueTypeIds() {
        return issueTypeIds;
    }

    public void setIssueTypeIds(List<Long> issueTypeIds) {
        this.issueTypeIds = issueTypeIds;
    }

    @Override
    public String toString() {
        return "ObjectSchemeFieldVO{" +
                "id=" + id +
                ", code='" + code + '\'' +
                ", name='" + name + '\'' +
                ", description='" + description + '\'' +
                ", fieldType='" + fieldType + '\'' +
                ", fieldTypeName='" + fieldTypeName + '\'' +
                ", system=" + system +
                ", required=" + required +
                ", requiredScope='" + requiredScope + '\'' +
                ", context='" + context + '\'' +
                ", contextName='" + contextName + '\'' +
                ", schemeCode='" + schemeCode + '\'' +
                ", defaultValue='" + defaultValue + '\'' +
                ", projectId=" + projectId +
                ", organizationId=" + organizationId +
                ", objectVersionNumber=" + objectVersionNumber +
                ", fieldOptions=" + fieldOptions +
                ", defaultValueObj=" + defaultValueObj +
                ", contexts=" + contexts +
                ", extraConfig=" + extraConfig +
                ", issueTypeVOList=" + issueTypeVOList +
                ", issueTypeIds=" + issueTypeIds +
                '}';
    }
}
