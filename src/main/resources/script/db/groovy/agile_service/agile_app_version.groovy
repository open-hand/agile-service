package script.db.groovy.agile_service

databaseChangeLog(logicalFilePath: 'script/db/agile_app_version.groovy') {
    changeSet(id: '2021-03-09-agile-app-version', author: 'kaiwen.li@hand-china.com') {
        createTable(tableName: "agile_app_version", remarks: '应用版本表') {
            column(name: 'id', type: 'BIGINT UNSIGNED', autoIncrement: true, remarks: 'id') {
                constraints(primaryKey: true)
            }
            column(name: 'group_id', type: 'VARCHAR(255)', remarks: 'group id')
            column(name: 'artifact_id', type: 'VARCHAR(255)', remarks: 'artifact id') {
                constraints(nullable: false)
            }
            column(name: 'version', type: 'VARCHAR(255)', remarks: '版本') {
                constraints(nullable: false)
            }
            column(name: 'version_alias', type: 'VARCHAR(255)', remarks: '版本别名')
            column(name: 'service_code', type: 'VARCHAR(64)', remarks: '服务编码') {
                constraints(nullable: false)
            }
            column(name: 'project_id', type: 'BIGINT UNSIGNED', remarks: 'project id') {
                constraints(nullable: false)
            }
            column(name: 'organization_id', type: 'BIGINT UNSIGNED', remarks: ' ') {
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