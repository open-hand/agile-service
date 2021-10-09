package script.db.groovy.agile_service
databaseChangeLog(logicalFilePath:'agile_gantt_dimension_rank.groovy') {
    changeSet(id: '2021-10-09-agile-gantt-dimension-rank', author: 'kaiwen.li@hand-china.com') {
        createTable(tableName: "agile_gantt_dimension_rank") {
            column(name: 'id', type: 'BIGINT UNSIGNED', autoIncrement: true, remarks: 'id') {
                constraints(primaryKey: true)
            }
            column(name: 'instance_id', type: 'BIGINT UNSIGNED', remarks: 'user id/sprint id') {
                constraints(nullable: false)
            }
            column(name: 'instance_type', type: 'VARCHAR(128)', remarks: 'instance_id的类型，assignee/冲刺') {
                constraints(nullable: false)
            }
            column(name: 'dimension', type: 'VARCHAR(128)', remarks: '业务维度，经办人/冲刺') {
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