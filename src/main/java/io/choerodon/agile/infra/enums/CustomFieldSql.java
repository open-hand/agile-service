package io.choerodon.agile.infra.enums;

import org.apache.commons.lang3.StringUtils;

/**
 * @author chihao.ran@hand-china.com
 * 2021/06/11 14:59
 */
public enum CustomFieldSql implements FieldSql {

    /**
     * 单选框，复选框，选择器(单选)，选择器(多选)，人员
     */
    ANALYSIS_OPTION("LEFT JOIN fd_field_option affo ON affv.option_id = affo.id",
            "affo.`value`, affo.id",
            "IFNULL(affo.`value`,'无')",
            "affo.id",
            FieldSql.DEFAULT),

    /**
     * 人员
     */
    ANALYSIS_MEMBER("",
            "affv.option_id",
            "null", "IFNULL(affv.option_id, 0)",
            FieldSql.USER),

    /**
     * 单选框，复选框，选择器(单选)，选择器(多选)，人员
     */
    COMPARED_OPTION("LEFT JOIN fd_field_option cffo ON cffv.option_id = cffo.id",
            "cffo.`value`, cffo.id",
            "IFNULL(cffo.`value`,'无')",
            "cffo.id",
            FieldSql.DEFAULT),

    /**
     * 人员
     */
    COMPARED_MEMBER("",
            "cffv.option_id",
            "null", "IFNULL(cffv.option_id, 0)",
            FieldSql.USER),
    ;


    String linkSql;
    String groupSql;
    String valueSql;
    String idSql;
    String valueType;

    CustomFieldSql(String linkSql, String groupSql, String valueSql, String idSql, String valueType) {
        this.linkSql = linkSql;
        this.groupSql = groupSql;
        this.valueSql = valueSql;
        this.idSql = idSql;
        this.valueType = valueType;
    }

    public static String getDefaultSql(String fieldCode, String type) {
        if (ANALYSIS.equals(type)){
            return convertSql(CUSTOM_DEFAULT_ANALYSIS_JOIN, fieldCode);
        } else if (COMPARED.equals(type)){
            return convertSql(CUSTOM_DEFAULT_COMPARED_JOIN, fieldCode);
        }
        return "";
    }

    public static String convertSql(String sql, String fieldCode){
        return sql.replaceAll("%s", fieldCode);
    }

    @Override
    public String getLinkSql() {
        return linkSql;
    }
    @Override
    public String getGroupSql() {
        return groupSql;
    }
    @Override
    public String getValueSql() {
        return valueSql;
    }
    @Override
    public String getIdSql() {
        return idSql;
    }

    @Override
    public String getValueType() {
        return valueType;
    }

    public static FieldSql get(String fieldType, String type) {
        if (StringUtils.isBlank(fieldType)) {
            return null;
        }
        if (ANALYSIS.equals(type)){
            return getAnalysisFieldSql(fieldType);
        } else if (COMPARED.equals(type)){
            return getComparedFieldSql(fieldType);
        }
        return null;
    }

    private static FieldSql getComparedFieldSql(String fieldType) {
        switch (fieldType) {
            case FieldType.RADIO:
            case FieldType.CHECKBOX:
            case FieldType.SINGLE:
            case FieldType.MULTIPLE:
                return COMPARED_OPTION;
            case FieldType.MEMBER:
            case FieldType.MULTI_MEMBER:
                return COMPARED_MEMBER;
            default:
                return null;
        }
    }

    private static FieldSql getAnalysisFieldSql(String fieldType) {
        switch (fieldType) {
            case FieldType.RADIO:
            case FieldType.CHECKBOX:
            case FieldType.SINGLE:
            case FieldType.MULTIPLE:
                return ANALYSIS_OPTION;
            case FieldType.MEMBER:
            case FieldType.MULTI_MEMBER:
                return ANALYSIS_MEMBER;
            default:
                return null;
        }
    }
}
