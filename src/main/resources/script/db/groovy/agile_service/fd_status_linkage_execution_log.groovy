package script.db.groovy.agile_service
databaseChangeLog(logicalFilePath:'fd_status_linkage_execution_log.groovy') {
    changeSet(id: '2021-08-02-fd-status-linkage-execution-log', author: 'ztxemail@163.com') {
        createTable(tableName: "fd_status_linkage_execution_log", remarks: '状态联动执行记录表') {
            column(name: 'id', type: 'BIGINT UNSIGNED', autoIncrement: true, remarks: 'id') {
                constraints(primaryKey: true)
            }

            column(name: 'status_code', type: 'VARCHAR(30)', remarks: '执行状态code(SUCCESS/LOOP)') {
                constraints(nullable: false)
            }

            column(name: 'content', type: 'VARCHAR(255)', remarks: '联动信息') {
                constraints(nullable: false)
            }

            column(name: 'pre_issue_id', type: 'BIGINT UNSIGNED', remarks: '上一个issue') {
                constraints(nullable: false)
            }

            column(name: 'cur_issue_id', type: 'BIGINT UNSIGNED', remarks: '当前的issue') {
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

    changeSet(author: 'ztxemail@163.com',id: '2021-08-17-fd-status-linkage-execution-log-add-column'){
        addColumn(tableName: 'fd_status_linkage_execution_log') {
            column(name: 'remark', type: 'VARCHAR(255)', remarks: '备注')
        }
    }
}