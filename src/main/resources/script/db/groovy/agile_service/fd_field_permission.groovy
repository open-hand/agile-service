package script.db.groovy.agile_service
databaseChangeLog(logicalFilePath: 'fd_field_permission.groovy') {
    changeSet(id: '2021-07-19-create-table-fd-field-permission', author: 'kaiwen.li@hand-china.com') {
        createTable(tableName: "fd_field_permission", remarks: '字段权限表') {
            column(name: 'id', type: 'BIGINT UNSIGNED', autoIncrement: true, remarks: 'ID') {
                constraints(primaryKey: true)
            }
            column(name: 'field_id', type: 'BIGINT UNSIGNED', remarks: '字段id') {
                constraints(nullable: false)
            }
            column(name: 'issue_type_id', type: 'BIGINT UNSIGNED', remarks: '问题类型id') {
                constraints(nullable: false)
            }
            column(name: 'role_member_id', type: 'BIGINT UNSIGNED', remarks: '角色或者成员id') {
                constraints(nullable: false)
            }
            column(name: 'type', type: 'VARCHAR(128)', remarks: '成员类型,role/user') {
                constraints(nullable: false)
            }
            column(name: 'permission', type: 'VARCHAR(128)', remarks: '权限类型,read/write') {
                constraints(nullable: false)
            }
            column(name: 'project_id', type: 'BIGINT UNSIGNED', remarks: '项目id') {
                constraints(nullable: false)
            }
            column(name: 'organization_id', type: 'BIGINT UNSIGNED', remarks: '组织id'){
                constraints(nullable: false)
            }

            column(name: "object_version_number", type: "BIGINT", defaultValue: "1")
            column(name: "created_by", type: "BIGINT", defaultValue: "0")
            column(name: "creation_date", type: "DATETIME", defaultValueComputed: "CURRENT_TIMESTAMP")
            column(name: "last_updated_by", type: "BIGINT", defaultValue: "0")
            column(name: "last_update_date", type: "DATETIME", defaultValueComputed: "CURRENT_TIMESTAMP")
        }
    }
}