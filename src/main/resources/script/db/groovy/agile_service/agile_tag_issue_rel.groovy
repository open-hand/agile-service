package script.db.groovy.agile_service
databaseChangeLog(logicalFilePath:'agile_tag_issue_rel.groovy') {
    changeSet(id: '2021-03-24-agile-tag-issue-rel', author: 'kaiwen.li@hand-china.com') {
        createTable(tableName: "agile_tag_issue_rel", remarks: 'issue和tag关系表') {
            column(name: 'id', type: 'BIGINT UNSIGNED', autoIncrement: true, remarks: 'id') {
                constraints(primaryKey: true)
            }
            column(name: 'issue_id', type: 'BIGINT UNSIGNED', remarks: '问题id') {
                constraints(nullable: false)
            }
            column(name: 'app_service_code', type: 'VARCHAR(255)', remarks: '应用服务code') {
                constraints(nullable: false)
            }
            column(name: 'tag_name', type: 'VARCHAR(255)', remarks: 'tag名称') {
                constraints(nullable: false)
            }
            column(name: 'tag_project_id', type: 'BIGINT UNSIGNED', remarks: 'tag的项目id') {
                constraints(nullable: false)
            }
            column(name: 'project_id', type: 'BIGINT UNSIGNED', remarks: '项目id') {
                constraints(nullable: false)
            }
            column(name: 'organization_id', type: 'BIGINT UNSIGNED', remarks: '组织id') {
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