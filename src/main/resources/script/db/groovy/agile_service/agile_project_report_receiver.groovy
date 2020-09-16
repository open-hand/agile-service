package script.db.groovy.agile_service

databaseChangeLog(logicalFilePath: 'script/db/agile_project_report_receiver.groovy') {
    changeSet(id: '2020-09-15-agile-project-report-cc', author: 'jiaxu.cui@hand-china.com') {
        createTable(tableName: "agile_project_report_receiver", remarks: '项目报表接收人') {
            column(name: 'id', type: 'BIGINT UNSIGNED', autoIncrement: true, remarks: '主键') {
                constraints(primaryKey: true)
            }
            column(name: 'project_id', type: 'BIGINT UNSIGNED', remarks: '项目id') {
                constraints(nullable: false)
            }
            column(name: 'project_report_id', type: 'BIGINT UNSIGNED', remarks: '项目报表Id') {
                constraints(nullable: false)
            }
            column(name: 'type', type: 'VARCHAR(50)', remarks: '接收人类型: TO|CC') {
                constraints(nullable: false)
            }
            column(name: 'to_id', type: 'BIGINT UNSIGNED', remarks: '收件人Id')
            column(name: 'cc_id', type: 'BIGINT UNSIGNED', remarks: '抄送人Id')

            column(name: "object_version_number", type: "BIGINT UNSIGNED", defaultValue: "1")
            column(name: "created_by", type: "BIGINT UNSIGNED", defaultValue: "0")
            column(name: "creation_date", type: "DATETIME", defaultValueComputed: "CURRENT_TIMESTAMP")
            column(name: "last_updated_by", type: "BIGINT UNSIGNED", defaultValue: "0")
            column(name: "last_update_date", type: "DATETIME", defaultValueComputed: "CURRENT_TIMESTAMP")
        }
    }
}