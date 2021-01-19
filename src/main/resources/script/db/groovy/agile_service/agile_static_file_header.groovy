package script.db.groovy.agile_service

databaseChangeLog(logicalFilePath: 'agile_static_file_header.groovy') {
    changeSet(id: '2021-01-13-agile-static-file-header', author: 'chihao.ran@hand-china.com') {
        createTable(tableName: "agile_static_file_header") {
            column(name: 'id', type: 'BIGINT UNSIGNED', autoIncrement: true, remarks: 'attachment id') {
                constraints(primaryKey: true)
            }
            column(name: 'project_id', type: 'BIGINT UNSIGNED', remarks: 'project id') {
                constraints(nullable: false)
            }
            column(name: 'organization_id', type: 'BIGINT UNSIGNED', remarks: 'organization id') {
                constraints(nullable: false)
            }
            column(name: 'url', type: 'VARCHAR(500)', remarks: 'url') {
                constraints(nullable: false)
            }
            column(name: 'file_name', type: 'VARCHAR(500)', remarks: 'file name') {
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