package script.db.groovy.agile_service
databaseChangeLog(logicalFilePath:'agile_tag_compare_history.groovy') {
    changeSet(id: '2021-04-19-agile-tag-compare-history', author: 'kaiwen.li@hand-china.com') {
        createTable(tableName: "agile_tag_compare_history") {
            column(name: 'id', type: 'BIGINT UNSIGNED', autoIncrement: true, remarks: 'id') {
                constraints(primaryKey: true)
            }
            column(name: 'version_tag_history_id', type: 'BIGINT UNSIGNED', remarks: '版本tag对比历史id') {
                constraints(nullable: false)
            }
            column(name: 'app_service_code', type: 'VARCHAR(255)', remarks: '应用服务code') {
                constraints(nullable: false)
            }
            column(name: 'source', type: 'VARCHAR(255)', remarks: 'tag对比的源，一般是比较新的tag') {
                constraints(nullable: false)
            }
            column(name: 'target', type: 'VARCHAR(255)', remarks: 'tag对比的目标，source是从target中切出来的，为空则拿source的所有commit')
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