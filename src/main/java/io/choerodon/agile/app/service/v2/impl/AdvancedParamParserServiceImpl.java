package io.choerodon.agile.app.service.v2.impl;

import static org.hzero.core.base.BaseConstants.ErrorCode.DATA_INVALID;
import static io.choerodon.agile.infra.enums.search.SearchConstant.Operation;

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

    /**
     * issue_id in (select issue_id from agile_component_issue_rel where component_id in (1,2,3) and additional condition )
     */
    public static final String SQL_LINKED_TABLE_IN_OR_NOT_IN = " %s %s ( select %s from %s where %s in ( %s ) %s) ";
    /**
     * issue_id in ( select instance_id from fd_field_value where project_id in ( 1 ) and field_id = 1 and option_id in ( 1 ) and scheme_code = 'agile_issue')
     */
    public static final String SQL_CUSTOM_FIELD_IN_OR_NOT_IN = " %s %s ( select instance_id from fd_field_value where project_id in ( %s ) and field_id = %s and option_id in ( %s ) and scheme_code = '%s') ";


    /**
     * ================日期===============
     */

    public static final String SQL_CUSTOM_FIELD_DATE_BETWEEN = " %s %s ( select instance_id from fd_field_value where project_id in ( %s ) and field_id = %s and %s >= '%s' and %s <= '%s' and scheme_code = '%s') ";

    public static final String SQL_CUSTOM_FIELD_DATE_IS_NULL_OR_NOT_NULL = " %s %s ( select instance_id from fd_field_value where project_id in ( %s ) and field_id = %s and date_value %s and scheme_code = '%s') ";

    public static final String SQL_DATE_BETWEEN = " ( %s >= '%s' and %s <= '%s' ) ";

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
        List<Condition> conditions = searchParamVO.getConditions();
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
                        generateSelectorSql(sqlBuilder, predefinedFieldMap, condition, projectIds, instanceType);
                    case TIME:
                    case DATETIME:
                    case DATE:
                        generateDateSql(sqlBuilder, predefinedFieldMap, condition, projectIds, instanceType);
                    case NUMBER:
                        //数字
                    case TEXT:
                    case INPUT:
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

    private void generateDateSql(StringBuilder sqlBuilder,
                                 Map<String, FieldTableVO> predefinedFieldMap,
                                 Condition condition,
                                 Set<Long> projectIds,
                                 InstanceType instanceType) {
        Field field = condition.getField();
        String operation = condition.getOperation();
        //时间 between/is_null/is_not_null
        Assert.isTrue(Operation.DATE_OPERATIONS.contains(operation), DATA_INVALID);
        Pair<Date, Date> datePair = getDateValues(condition);
        boolean isPredefined = field.getPredefined();
        String alias = "ai";
        if (isPredefined) {
            String fieldCode = field.getFieldCode();
            FieldTableVO fieldTable = predefinedFieldMap.get(fieldCode);
            Assert.notNull(fieldTable, DATA_INVALID);
            String column = buildDateColumn(fieldTable.getField(), alias, fieldCode);
            String patter = queryPatterByFieldCode(fieldCode);
            DateFormat dateFormat = new SimpleDateFormat(patter);
            appendPredefinedDateSql(sqlBuilder, operation, datePair, column, dateFormat);
        } else {
            String primaryKey = "issue_id";
            String mainTableFilterColumn = buildMainTableFilterColumn(primaryKey, alias);
            String schemeCode = instanceType.getSchemeCode();
            appendCustomDateSql(sqlBuilder, projectIds, operation, datePair, mainTableFilterColumn, field, schemeCode);
        }
    }

    private void appendCustomDateSql(StringBuilder sqlBuilder,
                                     Set<Long> projectIds,
                                     String operation,
                                     Pair<Date, Date> datePair,
                                     String mainTableFilterColumn,
                                     Field field,
                                     String schemeCode) {
        String patter = BaseConstants.Pattern.DATETIME;
        Long fieldId = field.getFieldId();
        String fieldType = field.getFieldType();
        //时间选择器格式化
        String columnName = "date_value";
        if (FieldTypeCnName.TIME.equals(FieldTypeCnName.ofCode(fieldType).get())) {
            patter = BaseConstants.Pattern.TIME_SS;
            columnName = "DATE_FORMAT(date_value, '%H:%i:%s')";
        }
        DateFormat dateFormat = new SimpleDateFormat(patter);
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
                                dateFormat.format(datePair.getFirst()),
                                columnName,
                                dateFormat.format(datePair.getSecond()),
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
                                " is null ",
                                schemeCode));
                break;
            default:
                // =, >, >=, <, <=
                break;
        }
    }

    private void appendPredefinedDateSql(StringBuilder sqlBuilder,
                                         String operation,
                                         Pair<Date, Date> datePair,
                                         String column,
                                         DateFormat dateFormat) {
        switch (Operation.valueOf(operation)) {
            case BETWEEN:
                sqlBuilder.append(
                        String.format(
                                SQL_DATE_BETWEEN,
                                column,
                                dateFormat.format(datePair.getFirst()),
                                column,
                                dateFormat.format(datePair.getSecond())));
                break;
            case IS_NOT_NULL:
                sqlBuilder.append(String.format(SQL_SELF_TABLE_IS_NOT_NULL, column));
                break;
            case IS_NULL:
                sqlBuilder.append(String.format(SQL_SELF_TABLE_IS_NULL, column));
                break;
            default:
                // =, >, >=, <, <=
                break;
        }
    }

    private String queryPatterByFieldCode(String fieldCode) {
        if (DATETIME_MM_FIELD_LIST.contains(fieldCode)) {
            return BaseConstants.Pattern.DATETIME_MM;
        } else {
            return BaseConstants.Pattern.DATETIME;
        }
    }

    private String buildDateColumn(String column, String alias, String fieldCode) {
        String thisColumn = buildMainTableFilterColumn(column, alias);
        if (DATETIME_MM_FIELD_LIST.contains(fieldCode)) {
            //需要处理特殊日期格式
            thisColumn = String.format(SQL_DATE_FORMATTER, thisColumn, SQL_YYYY_MM_DD_HH_MM);
        }
        return thisColumn;
    }

    private Pair<Date, Date> getDateValues(Condition condition) {
        Pair<Value, Value> pair = condition.getBetweenValues();
        String operation = condition.getOperation();
        if (Operation.isNotNull(operation) || Operation.isNull(operation)) {
            //为空或不为空 跳过
            return Pair.of(null, null);
        }
        Date firstDate = null;
        Date secondDate = null;
        if (pair == null) {
            Value value = condition.getValue();
            Assert.notNull(value, DATA_INVALID);
            firstDate = value.getValueDate();
            Assert.notNull(firstDate, DATA_INVALID);
        } else {
            Value firstValue = pair.getFirst();
            Value secondValue = pair.getSecond();
            Assert.notNull(firstValue, DATA_INVALID);
            Assert.notNull(secondValue, DATA_INVALID);
            firstDate = firstValue.getValueDate();
            secondDate = secondValue.getValueDate();
            Assert.notNull(firstDate, DATA_INVALID);
            Assert.notNull(secondDate, DATA_INVALID);
        }
        return Pair.of(firstDate, secondDate);
    }

    private void generateSelectorSql(StringBuilder sqlBuilder,
                                     Map<String, FieldTableVO> predefinedFieldMap,
                                     Condition condition,
                                     Set<Long> projectIds,
                                     InstanceType instanceType) {
        Field field = condition.getField();
        String operation = condition.getOperation();
        //选择器，只支持in, not in, is null, is not null
        Assert.isTrue(Operation.SELECTOR_OPERATIONS.contains(operation), DATA_INVALID);
        String linkedOperation = resetSelectorOperation(operation);
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
                    generateLinkedTableSql(sqlBuilder, linkedOperation, values, fieldTable, alias, "and relation_type = 'fix'");
                    break;
                case FieldCode.INFLUENCE_VERSION:
                    generateLinkedTableSql(sqlBuilder, linkedOperation, values, fieldTable, alias, "and relation_type = 'influence'");
                    break;
                default:
                    if (!isLinkedTable) {
                        //主表字段
                        String mainTableFilterColumn = buildMainTableFilterColumn(fieldTable.getField(), alias);
                        if (Operation.isNull(operation)) {
                            sqlBuilder.append(String.format(SQL_SELF_TABLE_ID_IS_NULL, mainTableFilterColumn, mainTableFilterColumn));
                        } else if (Operation.isNotNull(operation)) {
                            sqlBuilder.append(String.format(SQL_SELF_TABLE_ID_IS_NOT_NULL, mainTableFilterColumn, mainTableFilterColumn));
                        } else {
                            sqlBuilder.append(
                                    String.format(
                                            SQL_SELF_TABLE_IN_OR_NOT_IN,
                                            mainTableFilterColumn,
                                            operation,
                                            StringUtils.join(values, BaseConstants.Symbol.COMMA)));
                        }
                    } else {
                        //关联表
                        generateLinkedTableSql(sqlBuilder, linkedOperation, values, fieldTable, alias, "");
                    }
                    break;
            }
        } else {
            String primaryKey = "issue_id";
            String mainTableFilterColumn = buildMainTableFilterColumn(primaryKey, alias);
            Long fieldId = field.getFieldId();
            String schemeCode = instanceType.getSchemeCode();
            sqlBuilder.append(
                    String.format(
                            SQL_CUSTOM_FIELD_IN_OR_NOT_IN,
                            mainTableFilterColumn,
                            linkedOperation,
                            StringUtils.join(projectIds, BaseConstants.Symbol.COMMA),
                            fieldId,
                            StringUtils.join(values, BaseConstants.Symbol.COMMA),
                            schemeCode));
        }
    }

    private void generateLinkedTableSql(StringBuilder sqlBuilder,
                                        String operation,
                                        List<?> values,
                                        FieldTableVO fieldTable,
                                        String alias,
                                        String additionalCondition) {
        String dbColumn = fieldTable.getField();
        String primaryKey = "issue_id";
        String mainTableFilterColumn = buildMainTableFilterColumn(primaryKey, alias);
        String table = fieldTable.getTable();
        sqlBuilder.append(
                String.format(SQL_LINKED_TABLE_IN_OR_NOT_IN, mainTableFilterColumn, operation, primaryKey, table, dbColumn, StringUtils.join(values, BaseConstants.Symbol.COMMA), additionalCondition));
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

    private String resetSelectorOperation(String operation) {
        //关联关系时，选择器情况 is null == not in; is not null = in
        if (Operation.isNull(operation)) {
            operation = Operation.NOT_IN.toString();
        }
        if (Operation.isNotNull(operation)) {
            operation = Operation.IN.toString();
        }
        return operation;
    }

    private List<? extends Object> getOptionValues(Condition condition) {
        //todo list object tag处理
        List<? extends Object> values;
        Value value = condition.getValue();
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
