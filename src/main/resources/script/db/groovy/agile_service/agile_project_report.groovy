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
            column(name: 'title', type: 'VARCHAR(255)', remarks: '报表标题') {
                constraints(nullable: false)
            }
            column(name: 'description', type: 'text', remarks: '报表说明')
            column(name: 'status', type: 'VARCHAR(50)', remarks: '报表状态: ENABLE|DISABLE|FINISHED') {
                constraints(nullable: false)
            }
            column(name: 'report_data', type: 'text', remarks: '报表数据')
            column(name: 'recent_send_date', type: 'DATETIME', remarks: '最近发送时间')

            column(name: "object_version_number", type: "BIGINT UNSIGNED", defaultValue: "1")
            column(name: "created_by", type: "BIGINT UNSIGNED", defaultValue: "0")
            column(name: "creation_date", type: "DATETIME", defaultValueComputed: "CURRENT_TIMESTAMP")
            column(name: "last_updated_by", type: "BIGINT UNSIGNED", defaultValue: "0")
            column(name: "last_update_date", type: "DATETIME", defaultValueComputed: "CURRENT_TIMESTAMP")
        }
    }
}