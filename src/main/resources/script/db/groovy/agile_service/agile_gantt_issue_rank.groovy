package script.db.groovy.agile_service
databaseChangeLog(logicalFilePath:'agile_gantt_issue_rank.groovy') {
    changeSet(id: '2021-09-26-agile-gantt-issue-rank', author: 'kaiwen.li@hand-china.com') {
        createTable(tableName: "agile_gantt_issue_rank", remarks: '甘特图issue排序表') {
            column(name: 'id', type: 'BIGINT UNSIGNED', autoIncrement: true, remarks: 'id') {
                constraints(primaryKey: true)
            }
            column(name: 'issue_id', type: 'BIGINT UNSIGNED', remarks: '问题id') {
                constraints(nullable: false)
            }
            column(name: 'dimension', type: 'VARCHAR(128)', remarks: '业务维度，任务/经办人/冲刺/史诗') {
                constraints(nullable: false)
            }
            column(name: 'instance_id', type: 'BIGINT UNSIGNED', remarks: '业务维度id，issueId/经办人id/冲刺id/史诗id') {
                constraints(nullable: false)
            }
            column(name: 'instance_type', type: 'VARCHAR(128)', remarks: 'instance_id的类型，task/assignee/冲刺/史诗') {
                constraints(nullable: false)
            }
            column(name: 'rank', type: 'VARCHAR(255)', remarks: 'rank') {
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

    changeSet(id: '2022-06-15-agile-gantt-issue-rank-add-index', author: 'kaiwen.li@zknow.com') {
        createIndex(tableName: "agile_gantt_issue_rank", indexName: "idx_issue_id_dimension") {
            column(name: "issue_id")
            column(name: "dimension")
        }
    }

}