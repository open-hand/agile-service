package io.choerodon.agile.domain.entity;

import org.springframework.expression.EvaluationContext;
import org.springframework.expression.spel.support.StandardEvaluationContext;

/**
 * @author superlee
 * @since 2022-11-22
 */
public class SqlTemplateData {

    private String column;

    private String opt;

    private String value;

    private String mainTableCol;

    private String innerCol;

    private String table;

    private String projectCol;

    private String projectIdStr;

    private String dbColumn;

    private String innerOpt;

    private String valueStr;

    private String additionalCondition;

    private String schemeCode;

    private String fieldId;

    private String columnName;

    private String first;

    private String second;

    private String columnOpt;

    private String columnValue;

    private String optionIds;

    private String epicIdWithAlias;

    private String typeCode;

    private String parentIssueId;

    private String firstValue;

    private String secondValue;

    public String getFirstValue() {
        return firstValue;
    }

    public SqlTemplateData setFirstValue(String firstValue) {
        this.firstValue = firstValue;
        return this;
    }

    public String getSecondValue() {
        return secondValue;
    }

    public SqlTemplateData setSecondValue(String secondValue) {
        this.secondValue = secondValue;
        return this;
    }

    public String getEpicIdWithAlias() {
        return epicIdWithAlias;
    }

    public SqlTemplateData setEpicIdWithAlias(String epicIdWithAlias) {
        this.epicIdWithAlias = epicIdWithAlias;
        return this;
    }

    public String getTypeCode() {
        return typeCode;
    }

    public SqlTemplateData setTypeCode(String typeCode) {
        this.typeCode = typeCode;
        return this;
    }

    public String getParentIssueId() {
        return parentIssueId;
    }

    public SqlTemplateData setParentIssueId(String parentIssueId) {
        this.parentIssueId = parentIssueId;
        return this;
    }

    public String getOptionIds() {
        return optionIds;
    }

    public SqlTemplateData setOptionIds(String optionIds) {
        this.optionIds = optionIds;
        return this;
    }

    public String getColumnOpt() {
        return columnOpt;
    }

    public SqlTemplateData setColumnOpt(String columnOpt) {
        this.columnOpt = columnOpt;
        return this;
    }

    public String getColumnValue() {
        return columnValue;
    }

    public SqlTemplateData setColumnValue(String columnValue) {
        this.columnValue = columnValue;
        return this;
    }

    public String getFirst() {
        return first;
    }

    public SqlTemplateData setFirst(String first) {
        this.first = first;
        return this;
    }

    public String getSecond() {
        return second;
    }

    public SqlTemplateData setSecond(String second) {
        this.second = second;
        return this;
    }

    public String getColumnName() {
        return columnName;
    }

    public SqlTemplateData setColumnName(String columnName) {
        this.columnName = columnName;
        return this;
    }

    public String getFieldId() {
        return fieldId;
    }

    public SqlTemplateData setFieldId(String fieldId) {
        this.fieldId = fieldId;
        return this;
    }

    public String getColumn() {
        return column;
    }

    public SqlTemplateData setColumn(String column) {
        this.column = column;
        return this;
    }

    public String getOpt() {
        return opt;
    }

    public SqlTemplateData setOpt(String opt) {
        this.opt = opt;
        return this;
    }

    public String getValue() {
        return value;
    }

    public SqlTemplateData setValue(String value) {
        this.value = value;
        return this;
    }

    public String getMainTableCol() {
        return mainTableCol;
    }

    public SqlTemplateData setMainTableCol(String mainTableCol) {
        this.mainTableCol = mainTableCol;
        return this;
    }

    public String getInnerCol() {
        return innerCol;
    }

    public SqlTemplateData setInnerCol(String innerCol) {
        this.innerCol = innerCol;
        return this;
    }

    public String getTable() {
        return table;
    }

    public SqlTemplateData setTable(String table) {
        this.table = table;
        return this;
    }

    public String getProjectCol() {
        return projectCol;
    }

    public SqlTemplateData setProjectCol(String projectCol) {
        this.projectCol = projectCol;
        return this;
    }

    public String getProjectIdStr() {
        return projectIdStr;
    }

    public SqlTemplateData setProjectIdStr(String projectIdStr) {
        this.projectIdStr = projectIdStr;
        return this;
    }

    public String getDbColumn() {
        return dbColumn;
    }

    public SqlTemplateData setDbColumn(String dbColumn) {
        this.dbColumn = dbColumn;
        return this;
    }

    public String getInnerOpt() {
        return innerOpt;
    }

    public SqlTemplateData setInnerOpt(String innerOpt) {
        this.innerOpt = innerOpt;
        return this;
    }

    public String getValueStr() {
        return valueStr;
    }

    public SqlTemplateData setValueStr(String valueStr) {
        this.valueStr = valueStr;
        return this;
    }

    public String getAdditionalCondition() {
        return additionalCondition;
    }

    public SqlTemplateData setAdditionalCondition(String additionalCondition) {
        this.additionalCondition = additionalCondition;
        return this;
    }

    public String getSchemeCode() {
        return schemeCode;
    }

    public SqlTemplateData setSchemeCode(String schemeCode) {
        this.schemeCode = schemeCode;
        return this;
    }

    public EvaluationContext ofContext() {
        return new StandardEvaluationContext(this);
    }
}
