package script.db.groovy.agile_service
databaseChangeLog(logicalFilePath:'agile_wiki_relation.groovy') {
    changeSet(id: '2018-12-03-agile-wiki-relation', author: 'fuqianghuang01@gmail.com') {
        createTable(tableName: "agile_wiki_relation") {
            column(name: 'id', type: 'BIGINT UNSIGNED', autoIncrement: true, remarks: 'id') {
                constraints(primaryKey: true)
            }
            column(name: 'project_id', type: 'BIGINT UNSIGNED', remarks: '项目id') {
                constraints(nullable: false)
            }
            column(name: 'issue_id', type: 'BIGINT UNSIGNED', remarks: '问题id') {
                constraints(nullable: false)
            }
            column(name: 'wiki_name', type: 'text', remarks: 'wiki名称') {
                constraints(nullable: false)
            }
            column(name: 'wiki_url', type: 'text', remarks: 'wiki url') {
                constraints(nullable: false)
            }

            column(name: "object_version_number", type: "BIGINT UNSIGNED", defaultValue: "1")
            column(name: "created_by", type: "BIGINT UNSIGNED", defaultValue: "0")
            column(name: "creation_date", type: "DATETIME", defaultValueComputed: "CURRENT_TIMESTAMP")
            column(name: "last_updated_by", type: "BIGINT UNSIGNED", defaultValue: "0")
            column(name: "last_update_date", type: "DATETIME", defaultValueComputed: "CURRENT_TIMESTAMP")
        }
    }

    changeSet(id: '2019-06-25-agile-wiki-relation-add-column', author: 'fuqianghuang01@gmail.com') {
        addColumn(tableName: 'agile_wiki_relation') {
            column(name: 'space_id', type: 'BIGINT UNSIGNED', remarks: '空间id')
        }
    }

    changeSet(id: '2021-12-15-agile-wiki-relation-add-index', author: 'ztxemail@163.com') {
        createIndex(tableName: 'agile_wiki_relation', indexName: 'idx_project_id') {
            column(name: 'project_id')
        }
        createIndex(tableName: 'agile_wiki_relation', indexName: 'idx_issue_id') {
            column(name: 'issue_id')
        }
    }
}