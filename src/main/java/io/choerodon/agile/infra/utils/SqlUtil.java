package io.choerodon.agile.infra.utils;

import static io.choerodon.agile.infra.enums.search.SearchConstant.SqlTemplate.*;

import java.util.Arrays;
import java.util.List;
import java.util.Set;

import javax.annotation.Nullable;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;

import io.choerodon.agile.api.vo.FieldTableVO;
import io.choerodon.agile.domain.entity.SqlTemplateData;
import io.choerodon.agile.infra.enums.FieldCode;
import io.choerodon.agile.infra.enums.InstanceType;
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
    public static final String BACKLOG_ID = "backlog_id";

    public static final String PRIMARY_KEY_ISSUE = "issue_id";
    public static final String PRIMARY_KEY_BACKLOG = "id";

    public static final String ALIAS_ISSUE = "ai";
    public static final String ALIAS_BACKLOG = "bl";
    public static final String PROJECT_ID = "project_id";
    public static final String PROGRAM_ID = "program_id";

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
                                              List<?> values,
                                              FieldTableVO fieldTable,
                                              String additionalCondition,
                                              InstanceType instanceType) {
        String alias = InstanceType.ISSUE.equals(instanceType) ? ALIAS_ISSUE : ALIAS_BACKLOG;
        StringBuilder sqlBuilder = new StringBuilder();
        SearchConstant.Operation opt = SearchConstant.Operation.valueOf(operation);
        String mainTableFilterColumn = SqlUtil.buildMainTableFilterColumn(fieldTable.getField(), alias);
        SqlTemplateData data =
                new SqlTemplateData()
                        .setMainTableCol(mainTableFilterColumn)
                        .setOpt(opt.getOpt())
                        .setValue(StringUtils.join(values, BaseConstants.Symbol.COMMA))
                        .setAdditionalCondition(additionalCondition);
        switch (opt) {
            case IN:
            case NOT_IN:
                sqlBuilder.append(SearchConstant.SqlTemplate.fillInParam(data.ofContext(), SELF_TABLE_IN_OR_NOT_IN));
                break;
            case IS_NULL:
                sqlBuilder.append(SearchConstant.SqlTemplate.fillInParam(data.ofContext(), SELF_TABLE_ID_IS_NULL));
                break;
            case IS_NOT_NULL:
                sqlBuilder.append(SearchConstant.SqlTemplate.fillInParam(data.ofContext(), SELF_TABLE_ID_IS_NOT_NULL));
                break;
            default:
                break;
        }
        return sqlBuilder.toString();
    }

    /**
     * 生成关联表的sql,一般关联表都是instanceId关联
     * ai.issue_id in (select issue_id from table where column_id in (1,2,3))
     *
     * @param operation           操作符
     * @param values              id集合
     * @param fieldTable          筛选的系统字段的基本信息
     * @param projectIds          关联表的项目id集合，如果关联表没有项目id字段，可以传空
     * @param additionalCondition 额外的附加条件，比如修复的版本，影响的版本，and relation_type = 'fix'
     * @param innerColumn         子查询的内部返回字段的名称，ai.issue_id in (select issue_id from table where column_id in (1,2,3)) issue_id就是innerColumn
     * @param isProgram           是否是项目群，决定子查询里的是project_id还是program_id
     * @param instanceType        实例类型，决定主表的别名和主键
     * @return
     */
    public static String generateLinkedTableSql(String operation,
                                                List<?> values,
                                                FieldTableVO fieldTable,
                                                @Nullable Set<Long> projectIds,
                                                String additionalCondition,
                                                String innerColumn,
                                                boolean isProgram,
                                                InstanceType instanceType) {
        String alias = InstanceType.ISSUE.equals(instanceType) ? ALIAS_ISSUE : ALIAS_BACKLOG;
        StringBuilder sqlBuilder = new StringBuilder();
        String dbColumn = fieldTable.getField();
        String primaryKey = InstanceType.ISSUE.equals(instanceType) ? PRIMARY_KEY_ISSUE : PRIMARY_KEY_BACKLOG;
        if (innerColumn == null) {
            innerColumn = primaryKey;
        }
        String mainTableFilterColumn = SqlUtil.buildMainTableFilterColumn(primaryKey, alias);
        String table = fieldTable.getTable();
        SearchConstant.Operation opt = SearchConstant.Operation.valueOf(operation);
        SqlTemplateData data =
                new SqlTemplateData()
                        .setMainTableCol(mainTableFilterColumn)
                        .setOpt(opt.getOpt())
                        .setInnerCol(innerColumn)
                        .setTable(table)
                        .setDbColumn(dbColumn)
                        .setAdditionalCondition(additionalCondition);
        boolean linkedTableHasProjectId = CollectionUtils.isNotEmpty(projectIds);
        if (linkedTableHasProjectId) {
            String projectIdStr = StringUtils.join(projectIds, BaseConstants.Symbol.COMMA);
            data.setProjectIdStr(projectIdStr);
            if (!isProgram) {
                data.setProjectCol(PROJECT_ID);
            } else {
                data.setProjectCol(PROGRAM_ID);
            }
        }
        String sqlTemplate;
        switch (opt) {
            case IN:
            case NOT_IN:
                data.setValue(StringUtils.join(values, BaseConstants.Symbol.COMMA));
                sqlTemplate = linkedTableHasProjectId ? LINKED_TABLE_IN_OR_NOT_IN : LINKED_TABLE_IN_OR_NOT_IN_WITHOUT_PROJECT_ID;
                sqlBuilder.append(SearchConstant.SqlTemplate.fillInParam(data.ofContext(), sqlTemplate));
                break;
            case IS_NULL:
            case IS_NOT_NULL:
                sqlTemplate = linkedTableHasProjectId ? LINKED_TABLE_IS_NULL_OR_NOT_NULL : LINKED_TABLE_IS_NULL_OR_NOT_NULL_WITHOUT_PROJECT_ID;
                sqlBuilder.append(SearchConstant.SqlTemplate.fillInParam(data.ofContext(), sqlTemplate));
                break;
            default:
                break;
        }
        return sqlBuilder.toString();
    }

    //todo 考虑合并
    public static String appendPredefinedSql(String operation,
                                             Pair<String, String> dataPair,
                                             FieldTableVO fieldTable,
                                             Set<Long> projectIds,
                                             String additionalCondition,
                                             String innerColumn,
                                             boolean isProgram,
                                             InstanceType instanceType) {
        String alias = InstanceType.ISSUE.equals(instanceType) ? ALIAS_ISSUE : ALIAS_BACKLOG;
        StringBuilder sqlBuilder = new StringBuilder();
        String dbColumn = fieldTable.getField();
        String primaryKey = InstanceType.ISSUE.equals(instanceType) ? PRIMARY_KEY_ISSUE : PRIMARY_KEY_BACKLOG;
        if (innerColumn == null) {
            innerColumn = primaryKey;
        }
        String mainTableFilterColumn = SqlUtil.buildMainTableFilterColumn(primaryKey, alias);
        String table = fieldTable.getTable();
        String projectIdStr = StringUtils.join(projectIds, BaseConstants.Symbol.COMMA);
        SearchConstant.Operation opt = SearchConstant.Operation.valueOf(operation);
        String fieldCode = fieldTable.getName();
        String column = SqlUtil.buildColumnByCode(fieldTable.getField(), alias, fieldCode);
        SqlTemplateData data =
                new SqlTemplateData()
                        .setMainTableCol(mainTableFilterColumn)
                        .setOpt(opt.getOpt())
                        .setInnerCol(innerColumn)
                        .setTable(table)
                        .setProjectIdStr(projectIdStr)
                        .setDbColumn(dbColumn)
                        .setColumn(column)
                        .setAdditionalCondition(additionalCondition);
        if (!isProgram) {
            data.setProjectCol(PROJECT_ID);
        } else {
            data.setProjectCol(PROGRAM_ID);
        }
        String selfTable = InstanceType.ISSUE.equals(instanceType) ? SearchConstant.TABLE_AGILE_ISSUE : SearchConstant.TABLE_BACKLOG;
        boolean isLinkedTable = !selfTable.equals(table);
        switch (opt) {
            case BETWEEN:
                data.setFirstValue(dataPair.getFirst());
                data.setSecondValue(dataPair.getSecond());
                if (isLinkedTable) {
                    data.setColumn(fieldTable.getField());
                    sqlBuilder.append(SearchConstant.SqlTemplate.fillInParam(data.ofContext(), LINKED_TABLE_BETWEEN));
                } else {
                    sqlBuilder.append(SearchConstant.SqlTemplate.fillInParam(data.ofContext(), DATE_BETWEEN));
                }
                break;
            case IS_NOT_NULL:
                if (isLinkedTable) {
                    sqlBuilder.append(SearchConstant.SqlTemplate.fillInParam(data.ofContext(), LINKED_TABLE_IS_NULL_OR_NOT_NULL));
                } else {
                    sqlBuilder.append(SearchConstant.SqlTemplate.fillInParam(data.ofContext(), SELF_TABLE_IS_NOT_NULL));
                }
                break;
            case IS_NULL:
                if (isLinkedTable) {
                    sqlBuilder.append(SearchConstant.SqlTemplate.fillInParam(data.ofContext(), LINKED_TABLE_IS_NULL_OR_NOT_NULL));
                } else {
                    sqlBuilder.append(SearchConstant.SqlTemplate.fillInParam(data.ofContext(), SELF_TABLE_IS_NULL));
                }
                break;
            case EQUAL:
            case LIKE:
                String value;
                if (opt.equals(SearchConstant.Operation.LIKE)) {
                    value = SearchConstant.SqlTemplate.toLikeValueExp(dataPair.getFirst());
                } else {
                    value = dataPair.getFirst();
                }
                if (isLinkedTable) {
                    data.setOpt(SearchConstant.Operation.IN.getOpt()).setInnerOpt(opt.getOpt()).setValue(value);
                    sqlBuilder.append(SearchConstant.SqlTemplate.fillInParam(data.ofContext(), LINKED_TABLE_EQUAL));
                } else {
                    data.setValue(value);
                    sqlBuilder.append(SearchConstant.SqlTemplate.fillInParam(data.ofContext(), SELF_TABLE_EQUAL));
                }
                break;
            default:
                // =, >, >=, <, <=
                break;
        }
        return sqlBuilder.toString();
    }
}
