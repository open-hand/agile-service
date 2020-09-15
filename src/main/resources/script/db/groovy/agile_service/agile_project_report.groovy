package script.db.groovy.agile_service

databaseChangeLog(logicalFilePath: 'script/db/agile_project_report.groovy') {
    changeSet(id: '2020-09-15-agile-project-report', author: 'jiaxu.cui@hand-china.com') {
        createTable(tableName: "agile_project_report", remarks: '项目报表') {
            column(name: 'id', type: 'BIGINT UNSIGNED', autoIncrement: true, remarks: '主键') {
                constraints(primaryKey: true)
            }
            column(name: 'project_id', type: 'BIGINT UNSIGNED', remarks: '项目id') {
                constraints(nullable: false)
            }
            column(name: 'title', type: 'VARCHAR(120)', remarks: '报表标题') {
                constraints(nullable: false)
            }
            column(name: 'title', type: 'VARCHAR(30)', remarks: '报表状态') {
                constraints(nullable: false)
            }
            column(name: 'job_id', type: 'BIGINT UNSIGNED', remarks: '定时任务Id') {
                constraints(nullable: true)
            }
            column(name: 'receiver_id', type: 'BIGINT UNSIGNED', remarks: '收件人Id') {
                constraints(nullable: true)
            }
            column(name: 'report_data', type: 'text', remarks: '报表数据')

            column(name: "object_version_number", type: "BIGINT UNSIGNED", defaultValue: "1")
            column(name: "created_by", type: "BIGINT UNSIGNED", defaultValue: "0")
            column(name: "creation_date", type: "DATETIME", defaultValueComputed: "CURRENT_TIMESTAMP")
            column(name: "last_updated_by", type: "BIGINT UNSIGNED", defaultValue: "0")
            column(name: "last_update_date", type: "DATETIME", defaultValueComputed: "CURRENT_TIMESTAMP")
        }
    }
}