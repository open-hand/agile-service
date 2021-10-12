package script.db.groovy.agile_service

/**
 * @author zhaotianxin
 * @date 2021-10-12 11:43
 **/
databaseChangeLog(logicalFilePath: 'script/db/agile_issue_participant_rel.groovy') {
    changeSet(id: '2021-10-12-agile-issue-participant-rel', author: 'ztxemail@163.com') {
        createTable(tableName: "agile_issue_participant_rel") {
            column(name: 'id', type: 'BIGINT UNSIGNED', autoIncrement: true, remarks: '主键id') {
                constraints(primaryKey: true)
            }
            column(name: 'issue_id', type: 'BIGINT UNSIGNED', remarks: '问题id') {
                constraints(nullable: false)
            }
            column(name: 'issue_type_id', type: 'BIGINT UNSIGNED', remarks: '问题类型id') {
                constraints(nullable: false)
            }
            column(name: 'participant_id', type: 'BIGINT UNSIGNED', remarks: '参与人id') {
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
        createIndex(indexName: 'idx_project_id', tableName: 'agile_issue_participant_rel') {
            column(name: 'project_id')
        }
        createIndex(indexName: "idx_issue_id", tableName: "agile_issue_participant_rel") {
            column(name: "issue_id")
        }
    }
}
