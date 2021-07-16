package io.choerodon.agile.api.vo;

import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

import java.util.List;

import io.choerodon.mybatis.domain.AuditDomain;

/**
 * @author chihao.ran@hand-china.com
 * 2021/07/13 15:59
 */
public class FieldCascadeRuleVO extends AuditDomain {
    @ApiModelProperty(value = "级联规则id")
    @Encrypt
    private Long id;
    @ApiModelProperty(value = "问题类型id")
    @Encrypt
    private Long issueTypeId;
    @ApiModelProperty(value = "字段id")
    @Encrypt
    private Long fieldId;
    @ApiModelProperty(value = "字段编码")
    private String fieldCode;
    @ApiModelProperty(value = "字段名称")
    private String fieldName;
    @ApiModelProperty(value = "字段类型")
    private String fieldType;
    @ApiModelProperty(value = "字段是否为系统字段")
    private Boolean fieldSystem;
    @ApiModelProperty(value = "字段选项id")
    @Encrypt
    private Long fieldOptionId;
    @ApiModelProperty(value = "级联字段id")
    @Encrypt
    private Long cascadeFieldId;
    @ApiModelProperty(value = "是否隐藏")
    private Boolean hidden;
    @ApiModelProperty(value = "是否必输")
    private Boolean required;
    @ApiModelProperty(value = "组织id")
    private Long organizationId;
    @ApiModelProperty(value = "项目id")
    private Long projectId;
    @ApiModelProperty(value = "级联字段名称")
    private String cascadeFieldName;
    @ApiModelProperty(value = "级联字段编码")
    private String cascadeFieldCode;
    @ApiModelProperty(value = "级联字段类型")
    private String cascadeFieldType;
    @ApiModelProperty(value = "级联字段是否为系统字段")
    private Boolean cascadeFieldSystem;
    @ApiModelProperty(value = "默认值")
    private Object defaultValue;
    @ApiModelProperty(value = "默认值对象集合")
    private List<Object> defaultValueObjs;
    @ApiModelProperty(value = "默认值选项id")
    @Encrypt
    private List<Long> defaultIds;
    @ApiModelProperty(value = "字段可见选项list")
    private List<FieldCascadeRuleOptionVO> fieldCascadeRuleOptionList;

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public Long getIssueTypeId() {
        return issueTypeId;
    }

    public void setIssueTypeId(Long issueTypeId) {
        this.issueTypeId = issueTypeId;
    }

    public Long getFieldId() {
        return fieldId;
    }

    public void setFieldId(Long fieldId) {
        this.fieldId = fieldId;
    }

    public Long getFieldOptionId() {
        return fieldOptionId;
    }

    public void setFieldOptionId(Long fieldOptionId) {
        this.fieldOptionId = fieldOptionId;
    }

    public Long getCascadeFieldId() {
        return cascadeFieldId;
    }

    public void setCascadeFieldId(Long cascadeFieldId) {
        this.cascadeFieldId = cascadeFieldId;
    }

    public Boolean getHidden() {
        return hidden;
    }

    public void setHidden(Boolean hidden) {
        this.hidden = hidden;
    }

    public Boolean getRequired() {
        return required;
    }

    public void setRequired(Boolean required) {
        this.required = required;
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

    public List<FieldCascadeRuleOptionVO> getFieldCascadeRuleOptionList() {
        return fieldCascadeRuleOptionList;
    }

    public void setFieldCascadeRuleOptionList(List<FieldCascadeRuleOptionVO> fieldCascadeRuleOptionList) {
        this.fieldCascadeRuleOptionList = fieldCascadeRuleOptionList;
    }

    public String getCascadeFieldName() {
        return cascadeFieldName;
    }

    public void setCascadeFieldName(String cascadeFieldName) {
        this.cascadeFieldName = cascadeFieldName;
    }

    public String getCascadeFieldCode() {
        return cascadeFieldCode;
    }

    public void setCascadeFieldCode(String cascadeFieldCode) {
        this.cascadeFieldCode = cascadeFieldCode;
    }

    public String getCascadeFieldType() {
        return cascadeFieldType;
    }

    public void setCascadeFieldType(String cascadeFieldType) {
        this.cascadeFieldType = cascadeFieldType;
    }

    public Object getDefaultValue() {
        return defaultValue;
    }

    public void setDefaultValue(Object defaultValue) {
        this.defaultValue = defaultValue;
    }

    public List<Object> getDefaultValueObjs() {
        return defaultValueObjs;
    }

    public void setDefaultValueObjs(List<Object> defaultValueObjs) {
        this.defaultValueObjs = defaultValueObjs;
    }

    public List<Long> getDefaultIds() {
        return defaultIds;
    }

    public void setDefaultIds(List<Long> defaultIds) {
        this.defaultIds = defaultIds;
    }

    public String getFieldCode() {
        return fieldCode;
    }

    public void setFieldCode(String fieldCode) {
        this.fieldCode = fieldCode;
    }

    public String getFieldName() {
        return fieldName;
    }

    public void setFieldName(String fieldName) {
        this.fieldName = fieldName;
    }

    public String getFieldType() {
        return fieldType;
    }

    public void setFieldType(String fieldType) {
        this.fieldType = fieldType;
    }

    public Boolean getFieldSystem() {
        return fieldSystem;
    }

    public void setFieldSystem(Boolean fieldSystem) {
        this.fieldSystem = fieldSystem;
    }

    public Boolean getCascadeFieldSystem() {
        return cascadeFieldSystem;
    }

    public void setCascadeFieldSystem(Boolean cascadeFieldSystem) {
        this.cascadeFieldSystem = cascadeFieldSystem;
    }
}
