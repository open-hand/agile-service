package script.db.groovy.agile_service

databaseChangeLog(logicalFilePath: 'script/db/agile_publish_version_tag_rel.groovy') {
    changeSet(id: '2021-04-29-agile-publish-version-tag-rel', author: 'kaiwen.li@hand-china.com') {
        createTable(tableName: "agile_publish_version_tag_rel", remarks: '发布版本与tag关系表') {
            column(name: 'id', type: 'BIGINT UNSIGNED', autoIncrement: true, remarks: 'id') {
                constraints(primaryKey: true)
            }
            column(name: 'publish_version_id', type: 'BIGINT UNSIGNED', remarks: '发布版本id') {
                constraints(nullable: false)
            }
            column(name: 'tag_name', type: 'VARCHAR(255)', remarks: '描述') {
                constraints(nullable: false)
            }
            column(name: 'app_service_code', type: 'VARCHAR(255)', remarks: '描述') {
                constraints(nullable: false)
            }
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