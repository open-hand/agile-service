package script.db.groovy.agile_service

databaseChangeLog(logicalFilePath: 'fd_status_transfer_setting.groovy') {
    changeSet(author: 'ztxemail@163.com', id: '2020-08-12-create-table-status-transfer-setting') {
        createTable(tableName: 'fd_status_transfer_setting', remarks: '状态转换配置表') {
            column(name: 'id', type: 'BIGINT UNSIGNED', autoIncrement: 'true', remarks: 'ID,主键') {
                constraints(primaryKey: true)
            }
            column(name: 'issue_type_id', type: 'BIGINT UNSIGNED', remarks: '问题类型Id') {
                constraints(nullable: false)
            }
            column(name: 'project_id', type: 'BIGINT UNSIGNED', remarks: '项目id') {
                constraints(nullable: false)
            }
            column(name: 'status_id', type: 'BIGINT UNSIGNED', remarks: '状态Id') {
                constraints(nullable: false)
            }
            column(name: 'user_type', type: 'VARCHAR(255)', remarks: '用户类型') {
                constraints(nullable: false)
            }
            column(name: 'user_id', type: 'BIGINT UNSIGNED', remarks: '用户id')

            column(name: "object_version_number", type: "BIGINT UNSIGNED", defaultValue: "1")
            column(name: "created_by", type: "BIGINT UNSIGNED", defaultValue: "0")
            column(name: "creation_date", type: "DATETIME", defaultValueComputed: "CURRENT_TIMESTAMP")
            column(name: "last_updated_by", type: "BIGINT UNSIGNED", defaultValue: "0")
            column(name: "last_update_date", type: "DATETIME", defaultValueComputed: "CURRENT_TIMESTAMP")
        }
    }

    changeSet(author: 'ztxemail@163.com',id: '2020-09-14-fd-status-transfer-setting-add-index'){
        createIndex(indexName: "idx_issue_type_id", tableName: "fd_status_transfer_setting") {
            column(name: "issue_type_id")
        }
        createIndex(indexName: "idx_project_id", tableName: "fd_status_transfer_setting") {
            column(name: "project_id")
        }
        createIndex(indexName: "idx_status_id", tableName: "fd_status_transfer_setting") {
            column(name: "status_id")
        }
    }

    changeSet(author: 'ztxemail@163.com',id: '2021-03-23-fd-status-transfer-setting-add-column'){
        addColumn(tableName: 'fd_status_transfer_setting') {
            column(name: 'organization_id', type: 'BIGINT UNSIGNED', remarks: '组织Id', defaultValue: "0")
        }
    }

    changeSet(author: 'ztxemail@163.com',id: '2021-08-16-fd-status-transfer-setting-add-column'){
        addColumn(tableName: 'fd_status_transfer_setting') {
            column(name: 'is_verify_subissue_completed', type: 'TINYINT UNSIGNED', remarks: '是否校验任务项子级全到已解决状态', defaultValue: "0")
        }
    }

    changeSet(author: 'tianxin.zhao@zknow.com',id: '2022-08-17-fd-status-transfer-setting-fix-data'){

        sql(stripComments: true, splitStatements: true, endDelimiter: ';'){
            "DELETE fsrs FROM fd_status_transfer_setting fsrs WHERE fsrs.issue_type_id NOT IN (SELECT fit.id FROM fd_issue_type fit)"
        }

    }
}