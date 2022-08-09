package script.db.groovy.agile_service


databaseChangeLog(logicalFilePath: 'fd_issue_type_field.groovy') {
    changeSet(id: '2020-08-11-create-issue-type-field', author: 'superlee') {
        createTable(tableName: 'fd_issue_type_field', remarks: '问题类型模版内容表') {
            column(name: 'id', type: 'BIGINT UNSIGNED', autoIncrement: 'true', remarks: 'ID,主键') {
                constraints(primaryKey: 'true')
            }
            column(name: 'issue_type_id', type: 'BIGINT UNSIGNED', remarks: 'issue类型id') {
                constraints(nullable: false)
            }
            column(name: 'project_id', type: 'BIGINT UNSIGNED', remarks: '项目id') {
                constraints(nullable: false)
            }
            column(name: 'template', type: 'text', remarks: '模版内容')

            column(name: "object_version_number", type: "BIGINT UNSIGNED", defaultValue: "1")
            column(name: "created_by", type: "BIGINT UNSIGNED", defaultValue: "0")
            column(name: "creation_date", type: "DATETIME", defaultValueComputed: "CURRENT_TIMESTAMP")
            column(name: "last_updated_by", type: "BIGINT UNSIGNED", defaultValue: "0")
            column(name: "last_update_date", type: "DATETIME", defaultValueComputed: "CURRENT_TIMESTAMP")
        }
    }
}
