package io.choerodon.agile.infra.utils;

import static io.choerodon.agile.infra.enums.search.SearchConstant.SqlTemplate.DATE_FORMATTER;

import java.util.Arrays;
import java.util.List;

import io.choerodon.agile.infra.enums.FieldCode;

import org.hzero.core.base.BaseConstants;

/**
 * @author superlee
 * @since 2022-11-18
 */
public class SqlUtil {

    public static final String SINGLE_QUOT = "'";

    public static final String SQL_YYYY_MM_DD_HH_MM = "%Y-%m-%d %H:%i";

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
}
