package io.choerodon.agile.api.vo;

import org.hzero.starter.keyencrypt.core.Encrypt;

import java.util.List;

/**
 * @author superlee
 * @since 2020-08-10
 */
public class PageConfigFieldVO {

    @Encrypt
    private Long id;

    @Encrypt
    private Long fieldId;

    private String fieldName;

    private Object defaultValue;

    private Object defaultValueObj;

    private String fieldType;

    private Boolean required;

    private Boolean created;

    private Boolean edited;

    private String issueType;

    private String rank;

    private String createdLevel;

    private Long objectVersionNumber;

    private PageConfigFieldEditedVO pageConfigFieldEdited;

    private String fieldCode;

    private Boolean extraConfig;

    private List<FieldOptionVO> fieldOptions;

    private List<Object> defaultValueObjs;

    @Encrypt
    private Long issueTypeId;

    private Integer instanceCount;

    public Object getDefaultValueObj() {
        return defaultValueObj;
    }

    public void setDefaultValueObj(Object defaultValueObj) {
        this.defaultValueObj = defaultValueObj;
    }

    public List<FieldOptionVO> getFieldOptions() {
        return fieldOptions;
    }

    public void setFieldOptions(List<FieldOptionVO> fieldOptions) {
        this.fieldOptions = fieldOptions;
    }

    public Boolean getExtraConfig() {
        return extraConfig;
    }

    public void setExtraConfig(Boolean extraConfig) {
        this.extraConfig = extraConfig;
    }

    public String getFieldCode() {
        return fieldCode;
    }

    public void setFieldCode(String fieldCode) {
        this.fieldCode = fieldCode;
    }

    public PageConfigFieldEditedVO getPageConfigFieldEdited() {
        return pageConfigFieldEdited;
    }

    public void setPageConfigFieldEdited(PageConfigFieldEditedVO pageConfigFieldEdited) {
        this.pageConfigFieldEdited = pageConfigFieldEdited;
    }

    public String getFieldType() {
        return fieldType;
    }

    public void setFieldType(String fieldType) {
        this.fieldType = fieldType;
    }

    public Long getObjectVersionNumber() {
        return objectVersionNumber;
    }

    public void setObjectVersionNumber(Long objectVersionNumber) {
        this.objectVersionNumber = objectVersionNumber;
    }

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public Long getFieldId() {
        return fieldId;
    }

    public void setFieldId(Long fieldId) {
        this.fieldId = fieldId;
    }

    public String getFieldName() {
        return fieldName;
    }

    public void setFieldName(String fieldName) {
        this.fieldName = fieldName;
    }

    public Object getDefaultValue() {
        return defaultValue;
    }

    public void setDefaultValue(Object defaultValue) {
        this.defaultValue = defaultValue;
    }

    public Boolean getRequired() {
        return required;
    }

    public void setRequired(Boolean required) {
        this.required = required;
    }

    public Boolean getCreated() {
        return created;
    }

    public void setCreated(Boolean created) {
        this.created = created;
    }

    public Boolean getEdited() {
        return edited;
    }

    public void setEdited(Boolean edited) {
        this.edited = edited;
    }

    public String getIssueType() {
        return issueType;
    }

    public void setIssueType(String issueType) {
        this.issueType = issueType;
    }

    public String getRank() {
        return rank;
    }

    public void setRank(String rank) {
        this.rank = rank;
    }

    public String getCreatedLevel() {
        return createdLevel;
    }

    public void setCreatedLevel(String createdLevel) {
        this.createdLevel = createdLevel;
    }

    public List<Object> getDefaultValueObjs() {
        return defaultValueObjs;
    }

    public void setDefaultValueObjs(List<Object> defaultValueObjs) {
        this.defaultValueObjs = defaultValueObjs;
    }

    public Long getIssueTypeId() {
        return issueTypeId;
    }

    public void setIssueTypeId(Long issueTypeId) {
        this.issueTypeId = issueTypeId;
    }

    public Integer getInstanceCount() {
        return instanceCount;
    }

    public void setInstanceCount(Integer instanceCount) {
        this.instanceCount = instanceCount;
    }
}
