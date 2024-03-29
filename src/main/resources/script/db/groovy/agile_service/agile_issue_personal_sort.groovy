package script.db.groovy.agile_service
databaseChangeLog(logicalFilePath:'agile_issue_personal_sort.groovy') {
    changeSet(id: '2021-10-26-agile-issue-personal-sort', author: 'kaiwen.li@hand-china.com') {
        createTable(tableName: "agile_issue_personal_sort") {
            column(name: 'id', type: 'BIGINT UNSIGNED', autoIncrement: true, remarks: 'id') {
                constraints(primaryKey: true)
            }
            column(name: 'user_id', type: 'BIGINT UNSIGNED', remarks: '用户id') {
                constraints(nullable: false)
            }
            column(name: 'business_type', type: 'VARCHAR(128)', remarks: '业务类型') {
                constraints(nullable: false)
            }
            column(name: 'sort_json', type: 'TEXT', remarks: '排序json') {
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
        createIndex(tableName: "agile_issue_personal_sort", indexName: "agile_issue_personal_sort_n1") {
            column(name: "project_id", type: "BIGINT UNSIGNED")
        }
        createIndex(tableName: "agile_issue_personal_sort", indexName: "agile_issue_personal_sort_n2") {
            column(name: "user_id", type: "BIGINT UNSIGNED")
        }
    }


}