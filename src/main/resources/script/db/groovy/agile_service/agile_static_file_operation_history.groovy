package script.db.groovy.agile_service
databaseChangeLog(logicalFilePath:'agile_static_file_operation_history.groovy') {
    changeSet(id: '2021-01-21-agile_static_file_operation_history', author: 'chihao.ran@hand-china.com') {
        createTable(tableName: "agile_static_file_operation_history") {
            column(name: 'id', type: 'BIGINT UNSIGNED', autoIncrement: true, remarks: 'attachment id') {
                constraints(primaryKey: true)
            }
            column(name: 'project_id', type: 'BIGINT UNSIGNED', remarks: 'project id') {
                constraints(nullable: false)
            }
            column(name: 'organization_id', type: 'BIGINT UNSIGNED', remarks: 'organization id') {
                constraints(nullable: false)
            }
            column(name: 'static_file_id', type: 'BIGINT UNSIGNED', remarks: 'static file header id'){
                constraints(nullable: false)
            }
            column(name: 'action', type: 'VARCHAR(255)', remarks: 'action')
            column(name: 'status', type: 'VARCHAR(255)', remarks: 'status, (failed、success、doing)')
            column(name: 'user_id', type: 'BIGINT UNSIGNED', remarks: 'user id')
            column(name: 'error_message', type: 'VARCHAR(255)', remarks: 'error message')
            column(name: "object_version_number", type: "BIGINT UNSIGNED", defaultValue: "1")
            column(name: "created_by", type: "BIGINT UNSIGNED", defaultValue: "0")
            column(name: "creation_date", type: "DATETIME", defaultValueComputed: "CURRENT_TIMESTAMP")
            column(name: "last_updated_by", type: "BIGINT UNSIGNED", defaultValue: "0")
            column(name: "last_update_date", type: "DATETIME", defaultValueComputed: "CURRENT_TIMESTAMP")
        }
    }
}