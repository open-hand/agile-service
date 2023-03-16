package io.choerodon.agile.infra.enums;

import org.apache.commons.lang3.StringUtils;

import java.util.stream.Stream;


/**
 * @author chihao.ran@hand-china.com
 * 2021/06/11 14:59
 */
public enum SystemBaseFieldSql implements FieldSql {
    //问题类型
    ISSUE_TYPE("issueType",
            "LEFT JOIN fd_issue_type fit ON fit.id = ai.issue_type_id",
            "fit.id, fit.`name`",
            "IFNULL(fit.`name`,'无')", "fit.id", DEFAULT),

    //状态
    STATUS("status",
            "LEFT JOIN fd_status fs ON ai.status_id = fs.id",
            "fs.`name`, fs.id",
            "IFNULL(fs.`name`, '无' )", "fs.id", DEFAULT),
    //优先级
    PRIORITY("priority",
            "LEFT JOIN fd_priority fp ON ai.priority_id = fp.id",
            "fp.`name`, fp.id",
            "IFNULL(fp.`name`, '无' )", "fp.id", DEFAULT),
    //模块
    COMPONENT("component",
            "LEFT JOIN agile_component_issue_rel acir ON acir.issue_id = ai.issue_id\n" +
                    "LEFT JOIN agile_issue_component aic ON aic.component_id = acir.component_id",
            "aic.`name`, aic.component_id",
            "IFNULL(aic.`name`, '无' )", "IFNULL(aic.component_id, 0)", DEFAULT),
    //标签
    LABEL("label",
            "LEFT JOIN agile_label_issue_rel alir ON alir.issue_id = ai.issue_id\n" +
                    "LEFT JOIN agile_issue_label ail ON ail.label_id = alir.label_id",
            "ail.label_name, ail.label_id",
            "IFNULL(ail.label_name, '无')", "IFNULL(ail.label_id, 0)", DEFAULT),
    //影响版本
    INFLUENCE_VERSION("influenceVersion",
            "LEFT JOIN agile_version_issue_rel lai ON lai.issue_id = ai.issue_id AND lai.relation_type = 'influence'" +
                    "LEFT JOIN agile_product_version api ON api.version_id = lai.version_id",
            "api.version_id, api.`name`",
            "IFNULL(api.`name`,'无')", "IFNULL(api.version_id, 0)", DEFAULT),
    //修复版本
    FIX_VERSION("fixVersion",
            "LEFT JOIN agile_version_issue_rel laf ON laf.issue_id = ai.issue_id AND laf.relation_type = 'fix'" +
                    "LEFT JOIN agile_product_version apf ON apf.version_id = laf.version_id",
            "apf.version_id, apf.`name`",
            "IFNULL(apf.`name`,'无')", "IFNULL(apf.version_id, 0)", DEFAULT),
    //所属史诗
    EPIC("epic",
            "LEFT JOIN agile_issue aie ON aie.issue_id = ai.epic_id AND aie.type_code = 'issue_epic'",
            "aie.epic_name, aie.issue_id",
            "IFNULL(aie.epic_name, '未分配')", "aie.issue_id", DEFAULT),
    //所属冲刺
    SPRINT("sprint",
            "LEFT JOIN agile_issue_sprint_rel aisr ON ai.issue_id = aisr.issue_id\n" +
                    "LEFT JOIN agile_sprint asr ON asr.sprint_id = aisr.sprint_id",
            "asr.sprint_name, asr.sprint_id",
            "IFNULL(asr.sprint_name, '未分配')", "IFNULL(asr.sprint_id, 0)", DEFAULT),
    //报告人
    REPORTER("reporter", "", "ai.reporter_id",
            "null", "IFNULL(ai.reporter_id, 0)", USER),
    //经办人
    ASSIGNEE("assignee", "", "ai.assignee_id",
            "null", "IFNULL(ai.assignee_id, 0)", USER),
    //创建人
    CREATOR("created_user", "", "ai.created_by",
            "null", "IFNULL(ai.created_by, 0)", USER),
    //更新人
    UPDATOR("last_updated_user", "", "ai.last_updated_by",
            "null", "IFNULL(ai.last_updated_by, 0)", USER),
    //tag
    TAG("tag",
            "LEFT JOIN agile_tag_issue_rel atir ON atir.issue_id = ai.issue_id",
            "atir.app_service_code, atir.tag_name, atir.tag_project_id",
            "IF(atir.tag_name IS NULL, '无', CONCAT_WS(':', atir.app_service_code, atir.tag_name))",
            "atir.tag_project_id", DEFAULT),
    //主要负责人
    MAIN_RESPONSIBLE("mainResponsible", "", "ai.main_responsible_id",
            "null", "IFNULL(ai.main_responsible_id, '0L')", USER),
    //环境
    ENVIRONMENT("environment", "",
            "ai.environment",
            "(CASE ai.environment\n" +
                    "WHEN 'pro' THEN '生产环境'\n" +
                    "WHEN 'other' THEN '非生产环境'\n" +
                    "ELSE '无' END)", "null", DEFAULT),
    //参与人
    PARTICIPANT("participant",
            "LEFT JOIN agile_issue_participant_rel aipr ON aipr.issue_id = ai.issue_id",
            "aipr.participant_id",
            "null", "IFNULL(aipr.participant_id, 0)", USER),
    ;

    String fieldCode;
    String linkSql;
    String groupSql;
    String valueSql;
    String idSql;
    String valueType;

    SystemBaseFieldSql(String fieldCode, String linkSql, String groupSql, String valueSql, String idSql, String valueType) {
        this.fieldCode = fieldCode;
        this.linkSql = linkSql;
        this.groupSql = groupSql;
        this.valueSql = valueSql;
        this.idSql = idSql;
        this.valueType = valueType;
    }

    public String getFieldCode() {
        return fieldCode;
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

    public static FieldSql get(String fieldCode) {
        return Stream.of(SystemBaseFieldSql.values())
                .filter(item -> StringUtils.equals(item.getFieldCode(), fieldCode))
                .findFirst()
                .orElse(null);
    }
}
