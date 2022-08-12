package script.db.groovy.agile_service
databaseChangeLog(logicalFilePath:'agile_static_file_operation_history.groovy') {
    changeSet(id: '2021-01-21-agile-static-file-operation-history', author: 'chihao.ran@hand-china.com') {
        createTable(tableName: "agile_static_file_operation_history", remarks: '敏捷静态历史记录表') {
            column(name: 'id', type: 'BIGINT UNSIGNED', autoIncrement: true, remarks: 'id') {
                constraints(primaryKey: true)
            }
            column(name: 'project_id', type: 'BIGINT UNSIGNED', remarks: '项目id') {
                constraints(nullable: false)
            }
            column(name: 'organization_id', type: 'BIGINT UNSIGNED', remarks: '组织id') {
                constraints(nullable: false)
            }
            column(name: 'static_file_id', type: 'BIGINT UNSIGNED', remarks: '静态文件头id'){
                constraints(nullable: false)
            }
            column(name: 'action', type: 'VARCHAR(255)', remarks: '行为')
            column(name: 'status', type: 'VARCHAR(255)', remarks: '状态, (failed、success、doing)')
            column(name: 'user_id', type: 'BIGINT UNSIGNED', remarks: '用户id')
            column(name: 'error_message', type: 'VARCHAR(255)', remarks: '错误消息')

            column(name: "object_version_number", type: "BIGINT UNSIGNED", defaultValue: "1")
            column(name: "created_by", type: "BIGINT UNSIGNED", defaultValue: "0")
            column(name: "creation_date", type: "DATETIME", defaultValueComputed: "CURRENT_TIMESTAMP")
            column(name: "last_updated_by", type: "BIGINT UNSIGNED", defaultValue: "0")
            column(name: "last_update_date", type: "DATETIME", defaultValueComputed: "CURRENT_TIMESTAMP")
        }
    }
}