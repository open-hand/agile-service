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
    OPTION("LEFT JOIN fd_field_option ffo ON ffv.option_id = ffo.id",
            "ffo.`value`, ffo.id",
            "IFNULL(ffo.`value`,'无')",
            "ffo.id",
            FieldSql.DEFAULT),

    /**
     * 人员
     */
    MEMBER("",
            "ffv.option_id",
            "null", "IFNULL(ffv.option_id, 0)",
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

    public static FieldSql get(String fieldType) {
        if (StringUtils.isBlank(fieldType)) {
            return null;
        }
        switch (fieldType) {
            case FieldType.RADIO:
            case FieldType.CHECKBOX:
            case FieldType.SINGLE:
            case FieldType.MULTIPLE:
                return OPTION;
            case FieldType.MEMBER:
            case FieldType.MULTI_MEMBER:
                return MEMBER;
            default:
                return null;
        }
    }
}
