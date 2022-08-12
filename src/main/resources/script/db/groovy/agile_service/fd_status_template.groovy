package script.db.groovy.agile_service

/**
 * @author zhaotianxin
 * @date 2021-03-25 10:38
 **/
databaseChangeLog(logicalFilePath: 'fd_status_template.groovy') {
    changeSet(author: 'ztxemail@163.com', id: '2021-03-25-create-table-status-template') {
        createTable(tableName: 'fd_status_template', remarks: '状态模版配置表') {
            column(name: 'id', type: 'BIGINT UNSIGNED', autoIncrement: 'true', remarks: 'ID,主键') {
                constraints(primaryKey: true)
            }
            column(name: 'organization_id', type: 'BIGINT UNSIGNED', remarks: '组织Id') {
                constraints(nullable: false)
            }
            column(name: 'status_id', type: 'BIGINT UNSIGNED', remarks: '状态Id') {
                constraints(nullable: false)
            }
            column(name: 'is_template_completed', type: 'TINYINT UNSIGNED', remarks: 'is template completed')

            column(name: "object_version_number", type: "BIGINT UNSIGNED", defaultValue: "1")
            column(name: "created_by", type: "BIGINT UNSIGNED", defaultValue: "0")
            column(name: "creation_date", type: "DATETIME", defaultValueComputed: "CURRENT_TIMESTAMP")
            column(name: "last_updated_by", type: "BIGINT UNSIGNED", defaultValue: "0")
            column(name: "last_update_date", type: "DATETIME", defaultValueComputed: "CURRENT_TIMESTAMP")
        }
    }
}
