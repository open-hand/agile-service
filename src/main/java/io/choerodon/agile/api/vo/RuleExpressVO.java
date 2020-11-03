package io.choerodon.agile.api.vo;

import java.math.BigDecimal;
import java.util.Date;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

/**
 * @author jiaxu.cuI@hand-china.com
 */
@JsonInclude(JsonInclude.Include.NON_NULL)
public class RuleExpressVO {

    @ApiModelProperty("快速搜索字段编码")
    private String fieldCode;
    @ApiModelProperty("快速搜索表达式操作关系：and、or等")
    private String operation;
    @ApiModelProperty("与上一条表达式关系")
    private String relationshipWithPervious;
    @ApiModelProperty("是否为预定义字段，必填")
    private Boolean predefined;
    @ApiModelProperty("字段类型")
    private String fieldType;
    @ApiModelProperty("是否是当前时间")
    private Boolean nowFlag;
    @ApiModelProperty("是否允许小数")
    private Boolean allowDecimals;
    @ApiModelProperty("快速搜索值")
    private Object value;
    @ApiModelProperty("字符串快速搜索值")
    private String valueStr;
    @ApiModelProperty("字符串数组快速搜索值")
    private List<String> valueStrList;
    @ApiModelProperty("id快速搜索值")
    @Encrypt
    private Long valueId;
    @ApiModelProperty("id数组快速搜索值")
    @Encrypt
    private List<Long> valueIdList;
    @ApiModelProperty("数字快速搜索值")
    private Integer valueNum;
    @ApiModelProperty("数字数组快速搜索值")
    private List<Integer> valueNumList;
    @ApiModelProperty("数字(允许小数)快速搜索值")
    private BigDecimal valueDecimal;
    @ApiModelProperty("数字(允许小数)数组快速搜索值")
    private List<BigDecimal> valueDecimalList;
    @ApiModelProperty("bool快速搜索值")
    private Boolean valueBool;
    @ApiModelProperty("时间快速搜索值")
    private Date valueDate;
    @ApiModelProperty("时分秒快速搜索值")
    private String valueDateHms;
    @ApiModelProperty("项目idList")
    private List<Long> projectIdList;

    public List<Long> getProjectIdList() {
        return projectIdList;
    }

    public void setProjectIdList(List<Long> projectIdList) {
        this.projectIdList = projectIdList;
    }

    public Boolean getNowFlag() {
        return nowFlag;
    }

    public void setNowFlag(Boolean nowFlag) {
        this.nowFlag = nowFlag;
    }

    public Boolean getAllowDecimals() {
        return allowDecimals;
    }

    public void setAllowDecimals(Boolean allowDecimals) {
        this.allowDecimals = allowDecimals;
    }

    public String getValueDateHms() {
        return valueDateHms;
    }

    public void setValueDateHms(String valueDateHms) {
        this.valueDateHms = valueDateHms;
    }

    public BigDecimal getValueDecimal() {
        return valueDecimal;
    }

    public void setValueDecimal(BigDecimal valueDecimal) {
        this.valueDecimal = valueDecimal;
    }

    public List<BigDecimal> getValueDecimalList() {
        return valueDecimalList;
    }

    public void setValueDecimalList(List<BigDecimal> valueDecimalList) {
        this.valueDecimalList = valueDecimalList;
    }

    public Object getValue() {
        return value;
    }

    public void setValue(Object value) {
        this.value = value;
    }

    public String getFieldCode() {
        return fieldCode;
    }

    public void setFieldCode(String fieldCode) {
        this.fieldCode = fieldCode;
    }

    public String getOperation() {
        return operation;
    }

    public void setOperation(String operation) {
        this.operation = operation;
    }

    public String getValueStr() {
        return valueStr;
    }

    public void setValueStr(String valueStr) {
        this.valueStr = valueStr;
    }

    public Long getValueId() {
        return valueId;
    }

    public void setValueId(Long valueId) {
        this.valueId = valueId;
    }

    public List<Long> getValueIdList() {
        return valueIdList;
    }

    public void setValueIdList(List<Long> valueIdList) {
        this.valueIdList = valueIdList;
    }

    public Integer getValueNum() {
        return valueNum;
    }

    public void setValueNum(Integer valueNum) {
        this.valueNum = valueNum;
    }

    public Boolean getValueBool() {
        return valueBool;
    }

    public void setValueBool(Boolean valueBool) {
        this.valueBool = valueBool;
    }

    public String getRelationshipWithPervious() {
        return relationshipWithPervious;
    }

    public void setRelationshipWithPervious(String relationshipWithPervious) {
        this.relationshipWithPervious = relationshipWithPervious;
    }

    public Boolean getPredefined() {
        return predefined;
    }

    public void setPredefined(Boolean predefined) {
        this.predefined = predefined;
    }

    public String getFieldType() {
        return fieldType;
    }

    public void setFieldType(String fieldType) {
        this.fieldType = fieldType;
    }

    public List<String> getValueStrList() {
        return valueStrList;
    }

    public void setValueStrList(List<String> valueStrList) {
        this.valueStrList = valueStrList;
    }

    public List<Integer> getValueNumList() {
        return valueNumList;
    }

    public void setValueNumList(List<Integer> valueNumList) {
        this.valueNumList = valueNumList;
    }

    public Date getValueDate() {
        return valueDate;
    }

    public void setValueDate(Date valueDate) {
        this.valueDate = valueDate;
    }
}
