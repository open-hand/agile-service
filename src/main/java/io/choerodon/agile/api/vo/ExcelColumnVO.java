package io.choerodon.agile.api.vo;


import java.util.List;
import java.util.Map;

/**
 * @author superlee
 * @since 2020-11-12
 */
public class ExcelColumnVO {

    /**
     * 是否为自定义字段
     */
    private boolean customField;

    /**
     * 是否为日期类型
     */
    private boolean dateType;

    /**
     * 自定义字段是否允许保存多个值
     */
    private boolean multiValue;

    /**
     * 系统字段 {@link io.choerodon.agile.infra.enums.FieldCode}
     * 和自定义字段code
     */
    private String fieldCode;

    /**
     * 合法的预选值，用于excel校验
     */
    private List<String> predefinedValues;

    /**
     * 下拉框中值与id映射
     */
    private Map<String, Long> valueIdMap;

    private Map<String, IssueTypeVO> issueTypeMap;

    private Map<String, StatusVO> issueStatusMap;

    /**
     * 环境名称和code的映射
     */
    private Map<String, String> envNameCodeMap;

    private PageFieldViewUpdateVO customFieldDetail;

    public boolean isDateType() {
        return dateType;
    }

    public void setDateType(boolean dateType) {
        this.dateType = dateType;
    }

    public PageFieldViewUpdateVO getCustomFieldDetail() {
        return customFieldDetail;
    }

    public void setCustomFieldDetail(PageFieldViewUpdateVO customFieldDetail) {
        this.customFieldDetail = customFieldDetail;
    }

    public Map<String, IssueTypeVO> getIssueTypeMap() {
        return issueTypeMap;
    }

    public void setIssueTypeMap(Map<String, IssueTypeVO> issueTypeMap) {
        this.issueTypeMap = issueTypeMap;
    }

    public Map<String, Long> getValueIdMap() {
        return valueIdMap;
    }

    public void setValueIdMap(Map<String, Long> valueIdMap) {
        this.valueIdMap = valueIdMap;
    }

    public boolean isMultiValue() {
        return multiValue;
    }

    public void setMultiValue(boolean multiValue) {
        this.multiValue = multiValue;
    }

    public boolean isCustomField() {
        return customField;
    }

    public void setCustomField(boolean customField) {
        this.customField = customField;
    }

    public String getFieldCode() {
        return fieldCode;
    }

    public void setFieldCode(String fieldCode) {
        this.fieldCode = fieldCode;
    }

    public List<String> getPredefinedValues() {
        return predefinedValues;
    }

    public void setPredefinedValues(List<String> predefinedValues) {
        this.predefinedValues = predefinedValues;
    }

    public Map<String, StatusVO> getIssueStatusMap() {
        return issueStatusMap;
    }

    public void setIssueStatusMap(Map<String, StatusVO> issueStatusMap) {
        this.issueStatusMap = issueStatusMap;
    }

    public Map<String, String> getEnvNameCodeMap() {
        return envNameCodeMap;
    }

    public void setEnvNameCodeMap(Map<String, String> envNameCodeMap) {
        this.envNameCodeMap = envNameCodeMap;
    }
}
