package script.db.groovy.agile_service

databaseChangeLog(logicalFilePath: 'fd_object_scheme_field_extend.groovy') {
    changeSet(id: '2020-08-09-create-table-object-scheme-field-extend', author: 'superlee') {
        createTable(tableName: "fd_object_scheme_field_extend") {
            column(name: 'id', type: 'BIGINT UNSIGNED', autoIncrement: true, remarks: 'ID') {
                constraints(primaryKey: true)
            }
            column(name: 'issue_type_id', type: 'BIGINT UNSIGNED', remarks: 'issue类型id') {
                constraints(nullable: false)
            }
            column(name: 'issue_type', type: 'VARCHAR(64)', remarks: 'issue类型') {
                constraints(nullable: false)
            }

            column(name: 'organization_id', type: 'BIGINT UNSIGNED', remarks: '组织id') {
                constraints(nullable: false)
            }
            column(name: 'project_id', type: 'BIGINT UNSIGNED', remarks: '项目id')
            column(name: 'field_id', type: 'BIGINT UNSIGNED', remarks: '字段id') {
                constraints(nullable: false)
            }
            column(name: 'is_required', type: 'TINYINT UNSIGNED(3)', remarks: '是否必填', defaultValue: "1") {
                constraints(nullable: false)
            }
            column(name: 'is_created', type: 'TINYINT UNSIGNED(3)', remarks: '是否在创建页面显示', defaultValue: "1") {
                constraints(nullable: false)
            }
            column(name: 'is_edited', type: 'TINYINT UNSIGNED(3)', remarks: '是否在编辑页面显示', defaultValue: "1") {
                constraints(nullable: false)
            }
            column(name: 'rank', type: 'VARCHAR(255)', remarks: 'rank排序值') {
                constraints(nullable: false)
            }

            column(name: "OBJECT_VERSION_NUMBER", type: "BIGINT", defaultValue: "1")
            column(name: "CREATED_BY", type: "BIGINT", defaultValue: "0")
            column(name: "CREATION_DATE", type: "DATETIME", defaultValueComputed: "CURRENT_TIMESTAMP")
            column(name: "LAST_UPDATED_BY", type: "BIGINT", defaultValue: "0")
            column(name: "LAST_UPDATE_DATE", type: "DATETIME", defaultValueComputed: "CURRENT_TIMESTAMP")
        }
    }
}