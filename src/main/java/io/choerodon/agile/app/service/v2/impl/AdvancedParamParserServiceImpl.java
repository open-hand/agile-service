package io.choerodon.agile.app.service.v2.impl;

import static org.hzero.core.base.BaseConstants.ErrorCode.DATA_INVALID;
import static io.choerodon.agile.infra.enums.search.SearchConstant.Operation;

import java.math.BigDecimal;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.Assert;

import io.choerodon.agile.api.validator.AdvancedParamValidator;
import io.choerodon.agile.api.vo.FieldTableVO;
import io.choerodon.agile.api.vo.search.Field;
import io.choerodon.agile.api.vo.search.SearchParamVO;
import io.choerodon.agile.api.vo.search.Condition;
import io.choerodon.agile.api.vo.search.Value;
import io.choerodon.agile.app.service.v2.AdvancedParamParserService;
import io.choerodon.agile.infra.enums.FieldCode;
import io.choerodon.agile.infra.enums.FieldTypeCnName;
import io.choerodon.agile.infra.enums.InstanceType;
import io.choerodon.core.exception.CommonException;

import org.hzero.core.base.BaseConstants;
import org.hzero.core.util.Pair;


/**
 * @author superlee
 * @since 2022-11-02
 */
@Service
public class AdvancedParamParserServiceImpl implements AdvancedParamParserService {

    @Autowired
    private AdvancedParamValidator advancedParamValidator;

    private static final String TABLE_AGILE_ISSUE = "agile_issue";
    private static final String TABLE_AGILE_ISSUE_SPRINT_REL = "agile_issue_sprint_rel";
    private static final String TABLE_AGILE_COMPONENT_ISSUE_REL = "agile_component_issue_rel";
    private static final String TABLE_AGILE_LABEL_ISSUE_REL = "agile_label_issue_rel";
    private static final String TABLE_AGILE_VERSION_ISSUE_REL = "agile_version_issue_rel";
    private static final String TABLE_AGILE_ISSUE_PARTICIPANT_REL = "agile_issue_participant_rel";
    private static final String TABLE_AGILE_ISSUE_PRODUCT_REL = "agile_issue_product_rel";
    private static final String TABLE_AGILE_TAG_ISSUE_REL = "agile_tag_issue_rel";

    public static final String SQL_YYYY_MM_DD_HH_MM = "%Y-%m-%d %H:%i";

    public static final String SINGLE_QUOT = "'";

    /**
     * ==============下拉框=================
     */

    /**
     * priority_id in (1,2,3)
     */
    public static final String SQL_SELF_TABLE_IN_OR_NOT_IN = " %s %s ( %s ) ";
    /**
     * (priority_id = 0 or priority_id is null)
     */
    public static final String SQL_SELF_TABLE_ID_IS_NULL = " (%s = 0 or %s is null) ";
    /**
     * (priority_id != 0 and priority_id is not null)
     */
    public static final String SQL_SELF_TABLE_ID_IS_NOT_NULL = " (%s != 0 and %s is not null) ";

    public static final String SQL_SELF_TABLE_IS_NULL = " (%s is null) ";

    public static final String SQL_SELF_TABLE_IS_NOT_NULL = " (%s is not null) ";

    public static final String SQL_SELF_TABLE_EQUAL = " (%s %s %s) ";

    public static final String SQL_LIKE_VALUE = " CONCAT(CONCAT('%s' ,'%s') ,'%s') ";

    /**
     * issue_id in (select issue_id from agile_component_issue_rel where component_id in (1,2,3) and additional condition )
     */
    public static final String SQL_LINKED_TABLE_IN_OR_NOT_IN = " %s %s ( select %s from %s where project_id in (%s) and %s in ( %s ) %s) ";

    public static final String SQL_LINKED_TABLE_IS_NULL_OR_NOT_NULL = " %s %s ( select %s from %s where project_id in (%s) %s) ";
    /**
     * issue_id in ( select instance_id from fd_field_value where project_id in ( 1 ) and field_id = 1 and option_id in ( 1 ) and scheme_code = 'agile_issue')
     */
    public static final String SQL_CUSTOM_FIELD_IN_OR_NOT_IN = " %s %s ( select instance_id from fd_field_value where project_id in ( %s ) and field_id = %s and option_id in ( %s ) and scheme_code = '%s') ";

    public static final String SQL_CUSTOM_FIELD_IS_NULL_OR_NOT_NULL = " %s %s ( select instance_id from fd_field_value where project_id in ( %s ) and field_id = %s and scheme_code = '%s') ";

    public static final String SQL_CUSTOM_FIELD_EQUAL_OR_LIKE = " %s %s ( select instance_id from fd_field_value where project_id in ( %s ) and field_id = %s and %s %s %s and scheme_code = '%s') ";


    /**
     * ================日期===============
     */

    public static final String SQL_CUSTOM_FIELD_DATE_BETWEEN = " %s %s ( select instance_id from fd_field_value where project_id in ( %s ) and field_id = %s and %s >= %s and %s <= %s and scheme_code = '%s') ";

    public static final String SQL_CUSTOM_FIELD_DATE_IS_NULL_OR_NOT_NULL = " %s %s ( select instance_id from fd_field_value where project_id in ( %s ) and field_id = %s and %s %s and scheme_code = '%s') ";

    public static final String SQL_DATE_BETWEEN = " ( %s >= %s and %s <= %s ) ";

    public static final String SQL_DATE_FORMATTER = "DATE_FORMAT(%s, '%s')";


    protected static final Map<String, FieldTableVO> PREDEFINED_FIELD_TABLE_MAP;

    private static final List<String> DATETIME_MM_FIELD_LIST =
            Arrays.asList(
                    FieldCode.ESTIMATED_START_TIME,
                    FieldCode.ESTIMATED_END_TIME,
                    FieldCode.ACTUAL_START_TIME,
                    FieldCode.ACTUAL_END_TIME
            );


    static {
        List<FieldTableVO> fieldTableList = Arrays.asList(
                new FieldTableVO(FieldCode.ISSUE_TYPE, "issue_type_id", TABLE_AGILE_ISSUE),
                new FieldTableVO(FieldCode.STATUS, "status_id", TABLE_AGILE_ISSUE),
                new FieldTableVO(FieldCode.ASSIGNEE, "assignee_id", TABLE_AGILE_ISSUE),
                new FieldTableVO(FieldCode.SPRINT, "sprint_id", TABLE_AGILE_ISSUE_SPRINT_REL),
                new FieldTableVO(FieldCode.PRIORITY, "priority_id", TABLE_AGILE_ISSUE),
                //todo 特性&史诗
                new FieldTableVO(FieldCode.FEATURE, "feature_id", TABLE_AGILE_ISSUE),
                new FieldTableVO(FieldCode.REPORTER, "reporter_id", TABLE_AGILE_ISSUE),
                new FieldTableVO(FieldCode.COMPONENT, "component_id", TABLE_AGILE_COMPONENT_ISSUE_REL),
                new FieldTableVO(FieldCode.LABEL, "label_id", TABLE_AGILE_LABEL_ISSUE_REL),
                new FieldTableVO(FieldCode.FIX_VERSION, "version_id", TABLE_AGILE_VERSION_ISSUE_REL),
                new FieldTableVO(FieldCode.INFLUENCE_VERSION, "version_id", TABLE_AGILE_VERSION_ISSUE_REL),
                new FieldTableVO(FieldCode.EPIC, "epic_id", TABLE_AGILE_ISSUE),
                new FieldTableVO(FieldCode.CREATION_DATE, "creation_date", TABLE_AGILE_ISSUE),
                new FieldTableVO(FieldCode.LAST_UPDATE_DATE, "last_update_date", TABLE_AGILE_ISSUE),
                new FieldTableVO(FieldCode.ESTIMATED_START_TIME, "estimated_start_time", TABLE_AGILE_ISSUE),
                new FieldTableVO(FieldCode.ESTIMATED_END_TIME, "estimated_end_time", TABLE_AGILE_ISSUE),
                new FieldTableVO(FieldCode.ACTUAL_START_TIME, "actual_start_time", TABLE_AGILE_ISSUE),
                new FieldTableVO(FieldCode.ACTUAL_END_TIME, "actual_end_time", TABLE_AGILE_ISSUE),
                new FieldTableVO(FieldCode.MAIN_RESPONSIBLE, "main_responsible_id", TABLE_AGILE_ISSUE),
                new FieldTableVO(FieldCode.ENVIRONMENT, "environment", TABLE_AGILE_ISSUE),
                new FieldTableVO(FieldCode.CREATOR, "created_by", TABLE_AGILE_ISSUE),
                new FieldTableVO(FieldCode.UPDATOR, "last_updated_by", TABLE_AGILE_ISSUE),
                new FieldTableVO(FieldCode.PARTICIPANT, "participant_id", TABLE_AGILE_ISSUE_PARTICIPANT_REL),
                //todo 特殊处理
                new FieldTableVO(FieldCode.TAG, null, TABLE_AGILE_TAG_ISSUE_REL),
                new FieldTableVO(FieldCode.STORY_POINTS, "story_points", TABLE_AGILE_ISSUE),
                new FieldTableVO(FieldCode.REMAINING_TIME, "remaining_time", TABLE_AGILE_ISSUE),
                new FieldTableVO(FieldCode.ESTIMATE_TIME, "estimate_time", TABLE_AGILE_ISSUE),
                new FieldTableVO(FieldCode.PRODUCT, "product_id", TABLE_AGILE_ISSUE_PRODUCT_REL)
                //todo 是否关联燕千云单据
                //todo 关联燕千云单号
        );
        PREDEFINED_FIELD_TABLE_MAP =
                fieldTableList.stream().collect(Collectors.toMap(FieldTableVO::getName, Function.identity()));
    }

    @Override
    public String parse(InstanceType instanceType,
                        SearchParamVO searchParamVO,
                        Set<Long> projectIds) {
        advancedParamValidator.validate(searchParamVO);
        Map<String, FieldTableVO> predefinedFieldMap = buildPredefinedFieldMap();
        List<Condition> conditions = new ArrayList<>();
        conditions.addAll(searchParamVO.getConditions());
        conditions.addAll(searchParamVO.getAdvancedConditions());
        return generateSql(instanceType, projectIds, predefinedFieldMap, conditions);
    }

    private String generateSql(InstanceType instanceType,
                               Set<Long> projectIds,
                               Map<String, FieldTableVO> predefinedFieldMap,
                               List<Condition> conditions) {
        StringBuilder sqlBuilder = new StringBuilder();
        for (int i = 0; i < conditions.size(); i++) {
            Condition condition = conditions.get(i);
            Field field = condition.getField();
            String fieldType = field.getFieldType();
            String operation = condition.getOperation();
            if (i > 0) {
                //第一个不拼
                sqlBuilder.append(condition.getRelationship());
            }
            if (Operation.isBracket(operation)) {
                //括号，a && (b || c)，读取 b和c然后加括号
                appendBracket(instanceType, projectIds, predefinedFieldMap, sqlBuilder, condition);
            } else {
                FieldTypeCnName fieldTypeCnName = FieldTypeCnName.ofCode(fieldType).get();
                switch (fieldTypeCnName) {
                    case MEMBER:
                    case MULTI_MEMBER:
                    case MULTIPLE:
                    case SINGLE:
                    case RADIO:
                    case CHECKBOX:
                        sqlBuilder.append(generateSelectorSql(predefinedFieldMap, condition, projectIds, instanceType));
                        break;
                    case TIME:
                    case DATETIME:
                    case DATE:
                        sqlBuilder.append(generateSqlByClazz(predefinedFieldMap, condition, projectIds, instanceType, Date.class));
                        break;
                    case NUMBER:
                        sqlBuilder.append(generateSqlByClazz(predefinedFieldMap, condition, projectIds, instanceType, BigDecimal.class));
                        break;
                    case TEXT:
                    case INPUT:
                        sqlBuilder.append(generateSqlByClazz(predefinedFieldMap, condition, projectIds, instanceType, String.class));
                        break;
                    default:
                        break;
                }
            }
        }
        return sqlBuilder.toString();
    }

    private void appendBracket(InstanceType instanceType,
                               Set<Long> projectIds,
                               Map<String, FieldTableVO> predefinedFieldMap,
                               StringBuilder sqlBuilder,
                               Condition condition) {
        List<Condition> subConditions = condition.getSubConditions();
        sqlBuilder
                .append(BaseConstants.Symbol.LEFT_BRACE)
                .append(" ")
                .append(generateSql(instanceType, projectIds, predefinedFieldMap, subConditions))
                .append(" ")
                .append(BaseConstants.Symbol.RIGHT_BRACE);
    }

    private String generateSqlByClazz(Map<String, FieldTableVO> predefinedFieldMap,
                                      Condition condition,
                                      Set<Long> projectIds,
                                      InstanceType instanceType,
                                      Class clazz) {
        StringBuilder sqlBuilder = new StringBuilder();
        Field field = condition.getField();
        String operation = condition.getOperation();
        List<String> options = new ArrayList<>();
        //判断与类型相关的操作符
        if (clazz == Date.class || clazz == BigDecimal.class) {
            options.addAll(Operation.DATE_OR_NUMBER_OPERATIONS);
        } else if (clazz == String.class) {
            options.addAll(Operation.STRING_OPERATIONS);
        }
        Assert.isTrue(options.contains(operation), DATA_INVALID);
        String fieldCode = field.getFieldCode();
        Pair<String, String> dataPair = getPairValues(condition, clazz, field);
        boolean isPredefined = field.getPredefined();
        String alias = "ai";
        if (isPredefined) {
            FieldTableVO fieldTable = predefinedFieldMap.get(fieldCode);
            Assert.notNull(fieldTable, DATA_INVALID);
            String column = buildColumnByCode(fieldTable.getField(), alias, fieldCode);
            appendPredefinedDateSql(sqlBuilder, operation, dataPair, column);
        } else {
            String primaryKey = "issue_id";
            String mainTableFilterColumn = buildMainTableFilterColumn(primaryKey, alias);
            String schemeCode = instanceType.getSchemeCode();
            appendCustomDateSql(sqlBuilder, projectIds, operation, dataPair, mainTableFilterColumn, field, schemeCode);
        }
        return sqlBuilder.toString();
    }

    private void appendCustomDateSql(StringBuilder sqlBuilder,
                                     Set<Long> projectIds,
                                     String operation,
                                     Pair<String, String> datePair,
                                     String mainTableFilterColumn,
                                     Field field,
                                     String schemeCode) {
        Long fieldId = field.getFieldId();
        String fieldType = field.getFieldType();
        String columnName;
        if (FieldTypeCnName.TIME_TYPES.contains(fieldType)) {
            columnName = "date_value";
        } else if (FieldTypeCnName.NUMBER_TYPES.contains(fieldType)) {
            columnName = "number_value";
        } else if (FieldTypeCnName.INPUT.equals(fieldType)) {
            columnName = "string_value";
        } else if (FieldTypeCnName.TEXT.equals(fieldType)) {
            columnName = "text_value";
        } else {
            throw new CommonException("error.illegal.field.type");
        }
        if (FieldTypeCnName.TIME.getCode().equals(fieldType)) {
            //时间选择器格式化
            columnName = "DATE_FORMAT(" + columnName + ", '%H:%i:%s')";
        }
        switch (Operation.valueOf(operation)) {
            case BETWEEN:
                sqlBuilder.append(
                        String.format(
                                SQL_CUSTOM_FIELD_DATE_BETWEEN,
                                mainTableFilterColumn,
                                Operation.IN.toString(),
                                StringUtils.join(projectIds, BaseConstants.Symbol.COMMA),
                                fieldId,
                                columnName,
                                datePair.getFirst(),
                                columnName,
                                datePair.getSecond(),
                                schemeCode));
                break;
            case IS_NOT_NULL:
                sqlBuilder.append(
                        String.format(
                                SQL_CUSTOM_FIELD_DATE_IS_NULL_OR_NOT_NULL,
                                mainTableFilterColumn,
                                Operation.IN.toString(),
                                StringUtils.join(projectIds, BaseConstants.Symbol.COMMA),
                                fieldId,
                                columnName,
                                " is not null ",
                                schemeCode));
                break;
            case IS_NULL:
                sqlBuilder.append(
                        String.format(
                                SQL_CUSTOM_FIELD_DATE_IS_NULL_OR_NOT_NULL,
                                mainTableFilterColumn,
                                Operation.IN.toString(),
                                StringUtils.join(projectIds, BaseConstants.Symbol.COMMA),
                                fieldId,
                                columnName,
                                " is null ",
                                schemeCode));
                break;
            case EQUAL:
                String value = datePair.getFirst();
                sqlBuilder.append(
                        String.format(
                                SQL_CUSTOM_FIELD_EQUAL_OR_LIKE,
                                mainTableFilterColumn,
                                Operation.IN.toString(),
                                StringUtils.join(projectIds, BaseConstants.Symbol.COMMA),
                                fieldId,
                                columnName,
                                "=",
                                value,
                                schemeCode));
                break;
            case LIKE:
                String valueStr = String.format(SQL_LIKE_VALUE, "%", datePair.getFirst(), "%");
                sqlBuilder.append(
                        String.format(
                                SQL_CUSTOM_FIELD_EQUAL_OR_LIKE,
                                mainTableFilterColumn,
                                Operation.IN.toString(),
                                StringUtils.join(projectIds, BaseConstants.Symbol.COMMA),
                                fieldId,
                                columnName,
                                "like",
                                valueStr,
                                schemeCode));
                break;
            default:
                // =, >, >=, <, <=
                break;
        }
    }

    private void appendPredefinedDateSql(StringBuilder sqlBuilder,
                                         String operation,
                                         Pair<String, String> datePair,
                                         String column) {
        switch (Operation.valueOf(operation)) {
            case BETWEEN:
                sqlBuilder.append(
                        String.format(
                                SQL_DATE_BETWEEN,
                                column,
                                datePair.getFirst(),
                                column,
                                datePair.getSecond()));
                break;
            case IS_NOT_NULL:
                sqlBuilder.append(String.format(SQL_SELF_TABLE_IS_NOT_NULL, column));
                break;
            case IS_NULL:
                sqlBuilder.append(String.format(SQL_SELF_TABLE_IS_NULL, column));
                break;
            case EQUAL:
                String value = datePair.getFirst();
                sqlBuilder.append(String.format(SQL_SELF_TABLE_EQUAL, column, "=", value));
                break;
            case LIKE:
                String valueStr = String.format(SQL_LIKE_VALUE, "%", datePair.getFirst(), "%");
                sqlBuilder.append(String.format(SQL_SELF_TABLE_EQUAL, column, "like", valueStr));
                break;
            default:
                // =, >, >=, <, <=
                break;
        }
    }

    private String queryPatterByFieldCode(Field field) {
        String patter = BaseConstants.Pattern.DATETIME;
        if (field.getPredefined()) {
            //预定义
            String fieldCode = field.getFieldCode();
            if (DATETIME_MM_FIELD_LIST.contains(fieldCode)) {
                patter = BaseConstants.Pattern.DATETIME_MM;
            }
        } else {
            String fieldType = field.getFieldType();
            if (FieldTypeCnName.TIME.getCode().equals(fieldType)) {
                patter = BaseConstants.Pattern.TIME_SS;
            }
        }
        return patter;
    }

    private String buildColumnByCode(String column, String alias, String fieldCode) {
        String thisColumn = buildMainTableFilterColumn(column, alias);
        if (DATETIME_MM_FIELD_LIST.contains(fieldCode)) {
            //需要处理特殊日期格式
            thisColumn = String.format(SQL_DATE_FORMATTER, thisColumn, SQL_YYYY_MM_DD_HH_MM);
        }
        return thisColumn;
    }

    private Pair<String, String> getPairValues(Condition condition,
                                               Class clazz,
                                               Field field) {
        Pair<Value, Value> pair = condition.getBetweenValues();
        String operation = condition.getOperation();
        if (Operation.isNotNull(operation) || Operation.isNull(operation)) {
            //为空或不为空 跳过
            return Pair.of(null, null);
        }
        String first = null;
        String second = null;
        if (pair == null) {
            Value value = condition.getValue();
            Assert.notNull(value, DATA_INVALID);
            first = getValueByClazz(clazz, value, field);
        } else {
            Value firstValue = pair.getFirst();
            Value secondValue = pair.getSecond();
            Assert.notNull(firstValue, DATA_INVALID);
            Assert.notNull(secondValue, DATA_INVALID);
            first = getValueByClazz(clazz, firstValue, field);
            second = getValueByClazz(clazz, secondValue, field);
        }
        return Pair.of(first, second);
    }

    private String getValueByClazz(Class clazz, Value value, Field field) {
        String valueStr = null;
        if (clazz == Date.class) {
            String patter = queryPatterByFieldCode(field);
            DateFormat dateFormat = new SimpleDateFormat(patter);
            Date date = value.getValueDate();
            Assert.notNull(date, DATA_INVALID);
            //日期value需要拼上'
            valueStr = new StringBuilder(SINGLE_QUOT).append(dateFormat.format(date)).append(SINGLE_QUOT).toString();
        } else if (clazz == BigDecimal.class) {
            BigDecimal bigDecimal = value.getValueDecimal();
            Assert.notNull(bigDecimal, DATA_INVALID);
            valueStr = bigDecimal.toString();
        } else if (clazz == String.class) {
            valueStr = value.getValueStr();
            Assert.notNull(valueStr, DATA_INVALID);
            valueStr = new StringBuilder(SINGLE_QUOT).append(valueStr).append(SINGLE_QUOT).toString();
        }
        return valueStr;
    }

    private String generateSelectorSql(Map<String, FieldTableVO> predefinedFieldMap,
                                       Condition condition,
                                       Set<Long> projectIds,
                                       InstanceType instanceType) {
        StringBuilder sqlBuilder = new StringBuilder();
        Field field = condition.getField();
        String operation = condition.getOperation();
        //选择器，只支持in, not in, is null, is not null
        Assert.isTrue(Operation.SELECTOR_OPERATIONS.contains(operation), DATA_INVALID);
        List<? extends Object> values = getOptionValues(condition);
        boolean isPredefined = field.getPredefined();
        String alias = "ai";
        if (isPredefined) {
            //预定义字段
            String fieldCode = field.getFieldCode();
            FieldTableVO fieldTable = predefinedFieldMap.get(fieldCode);
            Assert.notNull(fieldTable, DATA_INVALID);
            boolean isLinkedTable = !TABLE_AGILE_ISSUE.equals(fieldTable.getTable());
            switch (fieldCode) {
                case FieldCode.TAG:
                    break;
                case FieldCode.FIX_VERSION:
                    sqlBuilder.append(generateLinkedTableSql(operation, values, fieldTable, alias, projectIds, "and relation_type = 'fix'"));
                    break;
                case FieldCode.INFLUENCE_VERSION:
                    sqlBuilder.append(generateLinkedTableSql(operation, values, fieldTable, alias, projectIds, "and relation_type = 'influence'"));
                    break;
                default:
                    if (!isLinkedTable) {
                        //主表字段
                        sqlBuilder.append(generateSelfTableSql(operation, values, alias, fieldTable));
                    } else {
                        //关联表
                        sqlBuilder.append(generateLinkedTableSql(operation, values, fieldTable, alias, projectIds, ""));
                    }
                    break;
            }
        } else {
            sqlBuilder.append(generateCustomFieldSelectorSql(alias, field, instanceType, operation, projectIds, values));
        }
        return sqlBuilder.toString();
    }

    private String generateCustomFieldSelectorSql(String alias,
                                                  Field field,
                                                  InstanceType instanceType,
                                                  String operation,
                                                  Set<Long> projectIds,
                                                  List<? extends Object> values) {
        StringBuilder sqlBuilder = new StringBuilder();
        String primaryKey = "issue_id";
        String mainTableFilterColumn = buildMainTableFilterColumn(primaryKey, alias);
        Long fieldId = field.getFieldId();
        String schemeCode = instanceType.getSchemeCode();
        Operation opt = Operation.valueOf(operation);
        String projectIdStr = StringUtils.join(projectIds, BaseConstants.Symbol.COMMA);
        switch (opt) {
            case IN:
            case NOT_IN:
                sqlBuilder.append(
                        String.format(
                                SQL_CUSTOM_FIELD_IN_OR_NOT_IN,
                                mainTableFilterColumn,
                                opt.getOpt(),
                                projectIdStr,
                                fieldId,
                                StringUtils.join(values, BaseConstants.Symbol.COMMA),
                                schemeCode));
                break;
            case IS_NULL:
            case IS_NOT_NULL:
                sqlBuilder.append(
                        String.format(
                                SQL_CUSTOM_FIELD_IS_NULL_OR_NOT_NULL,
                                mainTableFilterColumn,
                                opt.getOpt(),
                                projectIdStr,
                                fieldId,
                                schemeCode));
                break;
            default:
                break;
        }
        return sqlBuilder.toString();
    }

    private String generateSelfTableSql(String operation,
                                        List<?> values, String alias,
                                        FieldTableVO fieldTable) {
        StringBuilder sqlBuilder = new StringBuilder();
        Operation opt = Operation.valueOf(operation);
        String mainTableFilterColumn = buildMainTableFilterColumn(fieldTable.getField(), alias);
        switch (opt) {
            case IN:
            case NOT_IN:
                sqlBuilder.append(
                        String.format(
                                SQL_SELF_TABLE_IN_OR_NOT_IN,
                                mainTableFilterColumn,
                                opt.getOpt(),
                                StringUtils.join(values, BaseConstants.Symbol.COMMA)));
                break;
            case IS_NULL:
                sqlBuilder.append(String.format(SQL_SELF_TABLE_ID_IS_NULL, mainTableFilterColumn, mainTableFilterColumn));
                break;
            case IS_NOT_NULL:
                sqlBuilder.append(String.format(SQL_SELF_TABLE_ID_IS_NOT_NULL, mainTableFilterColumn, mainTableFilterColumn));
                break;
            default:
                break;
        }
        return sqlBuilder.toString();
    }

    private String generateLinkedTableSql(String operation,
                                          List<?> values,
                                          FieldTableVO fieldTable,
                                          String alias,
                                          Set<Long> projectIds,
                                          String additionalCondition) {
        StringBuilder sqlBuilder = new StringBuilder();
        String dbColumn = fieldTable.getField();
        String primaryKey = "issue_id";
        String mainTableFilterColumn = buildMainTableFilterColumn(primaryKey, alias);
        String table = fieldTable.getTable();
        String projectIdStr = StringUtils.join(projectIds, BaseConstants.Symbol.COMMA);
        Operation opt = Operation.valueOf(operation);
        switch (opt) {
            case IN:
            case NOT_IN:
                sqlBuilder.append(
                        String.format(SQL_LINKED_TABLE_IN_OR_NOT_IN, mainTableFilterColumn, opt.getOpt(), primaryKey, table, projectIdStr, dbColumn, StringUtils.join(values, BaseConstants.Symbol.COMMA), additionalCondition));
                break;
            case IS_NULL:
            case IS_NOT_NULL:
                sqlBuilder.append(
                        String.format(SQL_LINKED_TABLE_IS_NULL_OR_NOT_NULL, mainTableFilterColumn, opt.getOpt(), primaryKey, table, projectIdStr, additionalCondition));
                break;
            default:
                break;
        }
        return sqlBuilder.toString();
    }

    private String buildMainTableFilterColumn(String column,
                                              String alias) {

        String mainTableFilterColumn;
        if (alias == null) {
            mainTableFilterColumn = column;
        } else {
            mainTableFilterColumn = alias + BaseConstants.Symbol.POINT + column;
        }
        return mainTableFilterColumn;
    }

    private List<? extends Object> getOptionValues(Condition condition) {
        //todo list object tag处理
        List<? extends Object> values;
        Value value = condition.getValue();
        if (value == null) {
            return null;
        }
        Field field = condition.getField();
        boolean noEncryptFlag = Boolean.TRUE.equals(field.getNoEncryptFlag());
        if (noEncryptFlag) {
            values = value.getNoEncryptIdList();
        } else {
            values = value.getValueIdList();
        }
        return values;
    }

    private Map<String, FieldTableVO> buildPredefinedFieldMap() {
        Map<String, FieldTableVO> map = new HashMap<>(PREDEFINED_FIELD_TABLE_MAP);
        //todo 瀑布，需求，项目群
        return map;
    }


}
