package script.db.groovy.agile_service

databaseChangeLog(logicalFilePath: 'script/db/agile_publish_version_tree_closure.groovy') {
    changeSet(id: '2021-03-09-agile-publish-version-tree-closure', author: 'kaiwen.li@hand-china.com') {
        createTable(tableName: "agile_publish_version_tree_closure", remarks: '应用版本树闭包表，存储应用版本树形结构') {
            column(name: 'id', type: 'BIGINT UNSIGNED', autoIncrement: true, remarks: 'id') {
                constraints(primaryKey: true)
            }
            column(name: 'ancestor_id', type: 'BIGINT UNSIGNED', remarks: '祖先id') {
                constraints(nullable: false)
            }
            column(name: 'descendant_id', type: 'BIGINT UNSIGNED', remarks: '后代id') {
                constraints(nullable: false)
            }
            column(name: 'descendant_parent', type: 'BIGINT UNSIGNED', remarks: '后代对应的直属父级id') {
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