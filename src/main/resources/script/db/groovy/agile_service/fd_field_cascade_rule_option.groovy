package script.db.groovy.agile_service

databaseChangeLog(logicalFilePath: 'script/db/fd_field_cascade_rule_option.groovy') {
    changeSet(author: "chihao.ran@hand-china.com", id: "2021-07-15-fd-field-cascade-rule-option") {
        createTable(tableName: "fd_field_cascade_rule_option", remarks: "字段级联规则选项") {
            column(name: "id", type: "BIGINT UNSIGNED", autoIncrement: true, remarks: "主键") {
                constraints(primaryKey: true)
            }
            column(name: "field_cascade_rule_id", type: "BIGINT UNSIGNED", remarks: "字段级联规则id") {
                constraints(nullable: "false")
            }
            column(name: "cascade_option_id", type: "BIGINT UNSIGNED", remarks: "级联字段选项") {
                constraints(nullable: "false")
            }
            column(name: "is_default", type: "TINYINT UNSIGNED(1)", defaultValue: "0", remarks: "是否默认")
            column(name: "project_id", type: "BIGINT UNSIGNED", remarks: "项目id") {
                constraints(nullable: "false")
            }
            column(name: "organization_id", type: "BIGINT UNSIGNED", remarks: "组织id") {
                constraints(nullable: "false")
            }

            column(name: "created_by", type: "BIGINT UNSIGNED", defaultValue: "0", remarks: "创建人")
            column(name: "last_updated_by", type: "BIGINT UNSIGNED", defaultValue: "0", remarks: "最近更新人")
            column(name: "creation_date", type: "DATETIME", defaultValueComputed: "CURRENT_TIMESTAMP", remarks: "创建时间")
            column(name: "last_update_date", type: "DATETIME", defaultValueComputed: "CURRENT_TIMESTAMP", remarks: "最近更新时间")
            column(name: "object_version_number", type: "BIGINT UNSIGNED", defaultValue: "1", remarks: "行版本号，用来处理锁")
        }
    }
}
