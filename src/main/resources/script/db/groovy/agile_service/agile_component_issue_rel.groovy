package script.db.groovy.agile_service

databaseChangeLog(logicalFilePath: 'agile_component_issue_rel.groovy') {
    changeSet(id: '2018-05-14-agile-component-issue-rel', author: 'fuqianghuang01@gmail.com') {
        createTable(tableName: "agile_component_issue_rel") {
            column(name: 'component_id', type: 'BIGINT UNSIGNED', remarks: '模块id') {
                constraints(nullable: false)
            }
            column(name: 'issue_id', type: 'BIGINT UNSIGNED', remarks: '问题id') {
                constraints(nullable: false)
            }
            column(name: 'project_id', type: 'BIGINT UNSIGNED', remarks: '项目id') {
                constraints(nullable: false)
            }
            column(name: "object_version_number", type: "BIGINT UNSIGNED", defaultValue: "1")
            column(name: "created_by", type: "BIGINT UNSIGNED", defaultValue: "0")
            column(name: "creation_date", type: "DATETIME", defaultValueComputed: "CURRENT_TIMESTAMP")
            column(name: "last_updated_by", type: "BIGINT UNSIGNED", defaultValue: "0")
            column(name: "last_update_date", type: "DATETIME", defaultValueComputed: "CURRENT_TIMESTAMP")
        }
    }
    changeSet(id: '2018-06-06-agile-component-issue-rel-add-index', author: 'dinghuang123@gmail.com') {
        createIndex(indexName: "uk_component_id_issue_id", tableName: "agile_component_issue_rel", unique: true) {
            column(name: "component_id")
            column(name: "issue_id")
        }
    }
    changeSet(id: '2018-06-08-agile-component-issue-rel-drop-index', author: 'dinghuang123@gmail.com') {
        dropIndex(tableName: 'agile_component_issue_rel', indexName: 'uk_component_id_issue_id')
    }

    changeSet(id: '2018-12-05-component-issue-rel-add-index', author: 'fuqianghuang01@gmail.com') {
        createIndex(tableName: "agile_component_issue_rel", indexName: "idx_issue_id") {
            column(name: "issue_id")
        }
    }

    changeSet(id: '2020-05-13-agile-component-issue-rel-add-column', author: 'fuqianghuang01@gmail.com') {
        addColumn(tableName: 'agile_component_issue_rel') {
            column(name: 'id', type: 'BIGINT UNSIGNED', autoIncrement: true, remarks: 'id') {
                constraints(primaryKey: true)
            }
        }
    }
}