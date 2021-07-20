package io.choerodon.agile.api.vo;

import org.hzero.starter.keyencrypt.core.Encrypt;

import java.util.List;

/**
 * @author chihao.ran@hand-china.com
 * 2021/07/20 16:35
 */
public class PageTemplateFieldVO {
    @Encrypt
    private Long id;

    @Encrypt
    private Long fieldId;

    private String fieldName;

    private Object defaultValue;

    private Object defaultValueObj;

    private String fieldType;

    private String issueType;

    private String rank;

    private Long objectVersionNumber;

    private String fieldCode;

    private Boolean extraConfig;

    private List<FieldOptionVO> fieldOptions;

    private List<Object> defaultValueObjs;

    private List<FieldCascadeRuleDesVO> fieldCascadeRuleDesList;

    @Encrypt
    private Long issueTypeId;

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

    public List<FieldCascadeRuleDesVO> getFieldCascadeRuleDesList() {
        return fieldCascadeRuleDesList;
    }

    public void setFieldCascadeRuleDesList(List<FieldCascadeRuleDesVO> fieldCascadeRuleDesList) {
        this.fieldCascadeRuleDesList = fieldCascadeRuleDesList;
    }
}
