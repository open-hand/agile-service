package script.db.groovy.agile_service
databaseChangeLog(logicalFilePath:'agile_gantt_order.groovy') {
    changeSet(id: '2021-09-26-agile-gantt-order', author: 'kaiwen.li@hand-china.com') {
        createTable(tableName: "agile_gantt_order") {
            column(name: 'id', type: 'BIGINT UNSIGNED', autoIncrement: true, remarks: 'id') {
                constraints(primaryKey: true)
            }
            column(name: 'issue_id', type: 'BIGINT UNSIGNED', remarks: 'issue id') {
                constraints(nullable: false)
            }
            column(name: 'dimension', type: 'VARCHAR(128)', remarks: '业务维度，任务/经办人/冲刺/史诗') {
                constraints(nullable: false)
            }
            column(name: 'instance_id', type: 'BIGINT UNSIGNED', remarks: '业务维度id，issueId/经办人id/冲刺id/史诗id') {
                constraints(nullable: false)
            }
            column(name: 'instance_type', type: 'VARCHAR(128)', remarks: 'instance_id的类型，task/assignee/冲刺/史诗') {
                constraints(nullable: false)
            }
            column(name: 'rank', type: 'VARCHAR(255)', remarks: 'rank') {
                constraints(nullable: false)
            }
            column(name: 'project_id', type: 'BIGINT UNSIGNED', remarks: 'project id') {
                constraints(nullable: false)
            }
            column(name: 'organization_id', type: 'BIGINT UNSIGNED', remarks: 'organization id') {
                constraints(nullable: false)
            }

            column(name: "object_version_number", type: "BIGINT UNSIGNED", defaultValue: "1")
            column(name: "created_by", type: "BIGINT UNSIGNED", defaultValue: "0")
            column(name: "creation_date", type: "DATETIME", defaultValueComputed: "CURRENT_TIMESTAMP")
            column(name: "last_updated_by", type: "BIGINT UNSIGNED", defaultValue: "0")
            column(name: "last_update_date", type: "DATETIME", defaultValueComputed: "CURRENT_TIMESTAMP")
        }
    }


}