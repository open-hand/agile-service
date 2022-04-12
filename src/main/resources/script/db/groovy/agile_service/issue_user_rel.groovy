package script.db.groovy.agile_service

 databaseChangeLog(logicalFilePath: 'issue_user_rel.groovy') {
     changeSet(id: '2022-04-12-issue-user-rel', author: 'huaxin.deng@hand-china.com') {
        createTable(tableName: "issue_user_rel", remarks: '工作项用户关系表') {
            column(name: 'id', type: 'BIGINT UNSIGNED', autoIncrement: true, remarks: 'id') {
                constraints(primaryKey: true)
            }
            column(name: 'project_id', type: 'BIGINT UNSIGNED', remarks: '项目Id') {
                constraints(nullable: false)
            }
            column(name: 'issue_id', type: 'BIGINT UNSIGNED', remarks: 'issue id') {
                constraints(nullable: false)
            }
            column(name: 'user_id', type: 'BIGINT UNSIGNED', remarks: '用户id'){
                constraints(nullable: false)
            }
            column(name: 'user_type', type: 'VARCHAR(255)', remarks: '用户类型') {
                constraints(nullable: false)
            }
            column(name: 'organization_id', type: 'BIGINT UNSIGNED', remarks: '组织Id') {
                constraints(nullable: false)
            }

            column(name: "object_version_number", type: "BIGINT UNSIGNED", defaultValue: "1")
            column(name: "created_by", type: "BIGINT UNSIGNED", defaultValue: "0")
            column(name: "creation_date", type: "DATETIME", defaultValueComputed: "CURRENT_TIMESTAMP")
            column(name: "last_updated_by", type: "BIGINT UNSIGNED", defaultValue: "0")
            column(name: "last_update_date", type: "DATETIME", defaultValueComputed: "CURRENT_TIMESTAMP")
        }

         createIndex(indexName: "idx_project_id", tableName: "issue_user_rel") {
             column(name: "project_id")
         }
         createIndex(indexName: "idx_issue_id", tableName: "issue_user_rel") {
             column(name: "issue_id")
         }
         createIndex(indexName: "idx_user_id", tableName: "issue_user_rel") {
             column(name: "user_id")
         }
         createIndex(indexName: "uk_issue_id_user_id_user_type", unique: true, tableName: "issue_user_rel") {
             column(name: "issue_id")
             column(name: "user_id")
             column(name: "user_type")
         }
    }
 }