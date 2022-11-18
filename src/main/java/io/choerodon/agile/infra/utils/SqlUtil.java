package io.choerodon.agile.infra.utils;

import static io.choerodon.agile.infra.enums.search.SearchConstant.SqlTemplate.*;
import static io.choerodon.agile.infra.enums.search.SearchConstant.SqlTemplate.SELF_TABLE_ID_IS_NOT_NULL;

import java.util.*;

import org.apache.commons.lang3.StringUtils;

import io.choerodon.agile.api.vo.FieldTableVO;
import io.choerodon.agile.infra.enums.FieldCode;
import io.choerodon.agile.infra.enums.search.SearchConstant;

import org.hzero.core.base.BaseConstants;
import org.hzero.core.util.Pair;

/**
 * @author superlee
 * @since 2022-11-18
 */
public class SqlUtil {

    public static final String SINGLE_QUOT = "'";

    public static final String SQL_YYYY_MM_DD_HH_MM = "%Y-%m-%d %H:%i";

    public static final String INSTANCE_ID = "instance_id";
    public static final String DEFAULT_PRIMARY_KEY = "issue_id";

    public static final List<String> DATETIME_MM_FIELD_LIST =
            Arrays.asList(
                    FieldCode.ESTIMATED_START_TIME,
                    FieldCode.ESTIMATED_END_TIME,
                    FieldCode.ACTUAL_START_TIME,
                    FieldCode.ACTUAL_END_TIME
            );

    public static String appendSingleQuot(String value) {
        return new StringBuilder(SINGLE_QUOT).append(value).append(SINGLE_QUOT).toString();
    }

    public static String buildMainTableFilterColumn(String column,
                                                    String alias) {
        String mainTableFilterColumn;
        if (alias == null) {
            mainTableFilterColumn = column;
        } else {
            mainTableFilterColumn = alias + BaseConstants.Symbol.POINT + column;
        }
        return mainTableFilterColumn;
    }

    public static String buildColumnByCode(String column, String alias, String fieldCode) {
        String thisColumn = buildMainTableFilterColumn(column, alias);
        if (DATETIME_MM_FIELD_LIST.contains(fieldCode)) {
            //需要处理特殊日期格式
            thisColumn = String.format(DATE_FORMATTER, thisColumn, SQL_YYYY_MM_DD_HH_MM);
        }
        return thisColumn;
    }

    public static String generateSelfTableSql(String operation,
                                              List<?> values, String alias,
                                              FieldTableVO fieldTable,
                                              String additionalCondition) {
        StringBuilder sqlBuilder = new StringBuilder();
        SearchConstant.Operation opt = SearchConstant.Operation.valueOf(operation);
        String mainTableFilterColumn = SqlUtil.buildMainTableFilterColumn(fieldTable.getField(), alias);
        switch (opt) {
            case IN:
            case NOT_IN:
                sqlBuilder.append(
                        String.format(
                                SELF_TABLE_IN_OR_NOT_IN,
                                mainTableFilterColumn,
                                opt.getOpt(),
                                StringUtils.join(values, BaseConstants.Symbol.COMMA),
                                additionalCondition));
                break;
            case IS_NULL:
                sqlBuilder.append(String.format(SELF_TABLE_ID_IS_NULL, mainTableFilterColumn, mainTableFilterColumn, additionalCondition));
                break;
            case IS_NOT_NULL:
                sqlBuilder.append(String.format(SELF_TABLE_ID_IS_NOT_NULL, mainTableFilterColumn, mainTableFilterColumn, additionalCondition));
                break;
            default:
                break;
        }
        return sqlBuilder.toString();
    }

    public static String generateLinkedTableSql(String operation,
                                                List<?> values,
                                                FieldTableVO fieldTable,
                                                String alias,
                                                Set<Long> projectIds,
                                                String additionalCondition,
                                                String innerColumn,
                                                boolean isProgram) {
        StringBuilder sqlBuilder = new StringBuilder();
        String dbColumn = fieldTable.getField();
        String primaryKey = DEFAULT_PRIMARY_KEY;
        if (innerColumn == null) {
            innerColumn = primaryKey;
        }
        String mainTableFilterColumn = SqlUtil.buildMainTableFilterColumn(primaryKey, alias);
        String table = fieldTable.getTable();
        String projectIdStr = StringUtils.join(projectIds, BaseConstants.Symbol.COMMA);
        SearchConstant.Operation opt = SearchConstant.Operation.valueOf(operation);
        Map<String, String> dataMap = new HashMap<>();
        dataMap.put("mainTableCol", mainTableFilterColumn);
        dataMap.put("opt", opt.getOpt());
        dataMap.put("innerCol", innerColumn);
        dataMap.put("table", table);
        dataMap.put("projectIdStr", projectIdStr);
        dataMap.put("dbColumn", dbColumn);
        dataMap.put("additionalCondition", additionalCondition);
        if (!isProgram) {
            dataMap.put("projectCol", "project_id");
        } else {
            dataMap.put("projectCol", "program_id");
        }
        switch (opt) {
            case IN:
            case NOT_IN:
                dataMap.put("valueStr", StringUtils.join(values, BaseConstants.Symbol.COMMA));
                sqlBuilder.append(SearchConstant.SqlTemplate.fillInParam(dataMap, LINKED_TABLE_IN_OR_NOT_IN));
                break;
            case IS_NULL:
            case IS_NOT_NULL:
                sqlBuilder.append(SearchConstant.SqlTemplate.fillInParam(dataMap, LINKED_TABLE_IS_NULL_OR_NOT_NULL));
                break;
            default:
                break;
        }
        return sqlBuilder.toString();
    }

    public static String appendPredefinedSql(String operation,
                                             Pair<String, String> dataPair,
                                             String column) {
        StringBuilder sqlBuilder = new StringBuilder();
        switch (SearchConstant.Operation.valueOf(operation)) {
            case BETWEEN:
                sqlBuilder.append(
                        String.format(
                                DATE_BETWEEN,
                                column,
                                dataPair.getFirst(),
                                column,
                                dataPair.getSecond()));
                break;
            case IS_NOT_NULL:
                sqlBuilder.append(String.format(SELF_TABLE_IS_NOT_NULL, column));
                break;
            case IS_NULL:
                sqlBuilder.append(String.format(SELF_TABLE_IS_NULL, column));
                break;
            case EQUAL:
                String value = dataPair.getFirst();
                sqlBuilder.append(String.format(SELF_TABLE_EQUAL, column, "=", value));
                break;
            case LIKE:
                String valueStr = String.format(LIKE_VALUE, "%", dataPair.getFirst(), "%");
                sqlBuilder.append(String.format(SELF_TABLE_EQUAL, column, "like", valueStr));
                break;
            default:
                // =, >, >=, <, <=
                break;
        }
        return sqlBuilder.toString();
    }
}
