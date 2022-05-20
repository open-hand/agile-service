package script.db.groovy.agile_service
databaseChangeLog(logicalFilePath:'agile_issue_predecessor_tree_closure.groovy') {
    changeSet(id: '2021-11-15-agile-issue-predecessor-tree-closure', author: 'kaiwen.li@hand-china.com') {
        createTable(tableName: "agile_issue_predecessor_tree_closure") {
            column(name: 'id', type: 'BIGINT UNSIGNED', autoIncrement: true, remarks: 'id') {
                constraints(primaryKey: true)
            }
            column(name: 'ancestor_id', type: 'BIGINT UNSIGNED', remarks: '祖先id') {
                constraints(nullable: false)
            }
            column(name: 'descendant_id', type: 'BIGINT UNSIGNED', remarks: '后代id') {
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
        createIndex(tableName: "agile_issue_predecessor_tree_closure", indexName: "agile_issue_predecessor_tree_closure_n1") {
            column(name: "ancestor_id", type: 'BIGINT UNSIGNED')
        }
        createIndex(tableName: "agile_issue_predecessor_tree_closure", indexName: "agile_issue_predecessor_tree_closure_n2") {
            column(name: "descendant_id", type: 'BIGINT UNSIGNED')
        }
        createIndex(tableName: "agile_issue_predecessor_tree_closure", indexName: "agile_issue_predecessor_tree_closure_n3") {
            column(name: "project_id", type: 'BIGINT UNSIGNED')
        }
    }
}