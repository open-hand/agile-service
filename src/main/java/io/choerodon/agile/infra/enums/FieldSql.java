package io.choerodon.agile.infra.enums;

/**
 * @author chihao.ran@hand-china.com
 * 2021/06/22 9:48
 */
public interface FieldSql {

    String CUSTOM_DEFAULT_ANALYSIS_JOIN = "LEFT JOIN fd_object_scheme_field afosf ON afosf.CODE = '%s' AND (afosf.project_id = ai.project_id or afosf.project_id is null)\n" +
            "LEFT JOIN fd_field_value affv ON affv.instance_id = ai.issue_id AND affv.field_id = afosf.id";
    String CUSTOM_DEFAULT_COMPARED_JOIN = "LEFT JOIN fd_object_scheme_field cfosf ON cfosf.CODE = '%s' AND (cfosf.project_id = ai.project_id or cfosf.project_id is null)\n" +
            "LEFT JOIN fd_field_value cffv ON cffv.instance_id = ai.issue_id AND cffv.field_id = cfosf.id";
    String DEFAULT = "default";
    String USER = "user";
    String PROJECT = "project";
    String ANALYSIS = "analysis";
    String COMPARED = "compared";

    /**
     * 获取表连接sql
     * @return 表连接sql
     */
    String getLinkSql();

    /**
     * 分组sql
     * @return 分组sql
     */
    String getGroupSql();

    /**
     * 值表字段
     * @return 值表字段
     */
    String getValueSql();

    /**
     * id表字段
     * @return id表字段
     */
    String getIdSql();

    /**
     * 维度值类型
     * @return 维度值类型
     */
    String getValueType();
}
