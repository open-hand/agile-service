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

    changeSet(id: '2021-03-17-agile-app-version-add-columns', author: 'kaiwen.li@hand-china.com') {
        addColumn(tableName: 'agile_app_version') {
            column(name: 'app_service', type: 'TINYINT UNSIGNED(1)', remarks: '是否为应用服务的版本', defaultValue: false) {
                constraints(nullable: false)
            }
            column(name: 'tag', type: 'TINYINT UNSIGNED(1)', remarks: '是否为tag', defaultValue: false) {
                constraints(nullable: false)
            }
            column(name: 'parent_id', type: 'BIGINT UNSIGNED', remarks: '父节点id', defaultValue: '0') {
                constraints(nullable: false)
            }
        }
    }
}