package script.db.groovy.agile_service

databaseChangeLog(logicalFilePath: 'script/db/agile_publish_version.groovy') {
    changeSet(id: '2021-03-09-agile-publish-version', author: 'kaiwen.li@hand-china.com') {
        createTable(tableName: "agile_publish_version", remarks: '应用版本表') {
            column(name: 'id', type: 'BIGINT UNSIGNED', autoIncrement: true, remarks: 'id') {
                constraints(primaryKey: true)
            }
            column(name: 'group_id', type: 'VARCHAR(255)', remarks: 'group id')
            column(name: 'artifact_id', type: 'VARCHAR(255)', remarks: 'artifact id')
            column(name: 'version', type: 'VARCHAR(255)', remarks: '版本')
            column(name: 'version_alias', type: 'VARCHAR(255)', remarks: '版本别名')
            column(name: 'service_code', type: 'VARCHAR(64)', remarks: '服务编码')
            column(name: 'app_service', type: 'TINYINT UNSIGNED(1)', remarks: '是否为应用服务的版本', defaultValue: false) {
                constraints(nullable: false)
            }
            column(name: "actual_publish_date", type: "DATETIME", remarks: '实际发布时间')
            column(name: 'tag_id', type: 'BIGINT UNSIGNED', remarks: 'tag id')
            column(name: 'description', type: 'text', remarks: '描述')
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