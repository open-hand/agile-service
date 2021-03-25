package script.db.groovy.agile_service
databaseChangeLog(logicalFilePath:'agile_tag.groovy') {
    changeSet(id: '2021-03-24-agile-tag', author: 'kaiwen.li@hand-china.com') {
        createTable(tableName: "agile_tag") {
            column(name: 'id', type: 'BIGINT UNSIGNED', autoIncrement: true, remarks: 'id') {
                constraints(primaryKey: true)
            }
            column(name: 'name', type: 'VARCHAR(255)', remarks: 'tag名称') {
                constraints(nullable: false)
            }
            column(name: 'create_from', type: 'VARCHAR(255)', remarks: '从哪里创建的，branch/tag/commit SHA') {
                constraints(nullable: false)
            }
            column(name: 'message', type: 'text', remarks: 'tag消息')
            column(name: 'release_note', type: 'text', remarks: 'tag的发布说明')
            column(name: 'app_service_code', type: 'VARCHAR(64)', remarks: '应用服务编码') {
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