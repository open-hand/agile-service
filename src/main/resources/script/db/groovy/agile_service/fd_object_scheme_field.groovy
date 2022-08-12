package script.db.groovy.agile_service


databaseChangeLog(logicalFilePath: 'fd_object_scheme_field.groovy') {
    changeSet(id: '2019-03-29-create-table-object-scheme-field', author: 'shinan.chenX@gmail.com') {
        createTable(tableName: "fd_object_scheme_field", remarks: '字段表') {
            column(name: 'id', type: 'BIGINT UNSIGNED', autoIncrement: true, remarks: 'ID') {
                constraints(primaryKey: true)
            }
            column(name: 'code', type: 'VARCHAR(30)', remarks: '字段code') {
                constraints(nullable: false)
            }
            column(name: 'name', type: 'VARCHAR(255)', remarks: '名称') {
                constraints(nullable: false)
            }
            column(name: 'description', type: 'VARCHAR(255)', remarks: '描述')
            column(name: 'field_type', type: 'VARCHAR(30)', remarks: '类型') {
                constraints(nullable: false)
            }
            column(name: 'default_value', type: 'VARCHAR(255)', remarks: '默认值')
            column(name: 'extra_config', type: 'TINYINT UNSIGNED(1)', remarks: '额外配置（是否当前时间/是否包括小数）')
            column(name: 'is_required', type: 'TINYINT UNSIGNED(1)', remarks: '是否必填', defaultValue: "0") {
                constraints(nullable: false)
            }
            column(name: 'is_system', type: 'TINYINT UNSIGNED(1)', remarks: '是否系统字段', defaultValue: "0") {
                constraints(nullable: false)
            }
            column(name: 'context', type: 'VARCHAR(100)', remarks: '字段上下文（可多选）') {
                constraints(nullable: false)
            }
            column(name: 'scheme_code', type: 'VARCHAR(30)', remarks: '方案编码') {
                constraints(nullable: false)
            }
            column(name: 'project_id', type: 'BIGINT UNSIGNED', remarks: '项目id')
            column(name: 'organization_id', type: 'BIGINT UNSIGNED', remarks: '组织id'){
                constraints(nullable: false)
            }
            column(name: "OBJECT_VERSION_NUMBER", type: "BIGINT", defaultValue: "1")
            column(name: "CREATED_BY", type: "BIGINT", defaultValue: "-1")
            column(name: "CREATION_DATE", type: "DATETIME", defaultValueComputed: "CURRENT_TIMESTAMP")
            column(name: "LAST_UPDATED_BY", type: "BIGINT", defaultValue: "-1")
            column(name: "LAST_UPDATE_DATE", type: "DATETIME", defaultValueComputed: "CURRENT_TIMESTAMP")
        }
        createIndex(tableName: "fd_object_scheme_field", indexName: "pk_object_scheme_field_id") {
            column(name: 'id', type: 'BIGINT UNSIGNED')
        }
        createIndex(tableName: "fd_object_scheme_field", indexName: "idx_object_scheme_field_code") {
            column(name: "code", type: "VARCHAR(30)")
        }
        createIndex(tableName: "fd_object_scheme_field", indexName: "idx_object_scheme_field_scheme_code") {
            column(name: "scheme_code", type: "VARCHAR(30)")
        }
        createIndex(tableName: "fd_object_scheme_field", indexName: "idx_object_scheme_field_is_system") {
            column(name: "is_system", type: "TINYINT UNSIGNED(1)")
        }
        createIndex(tableName: "fd_object_scheme_field", indexName: "idx_object_scheme_field_project_id") {
            column(name: "project_id", type: "BIGINT UNSIGNED")
        }
        createIndex(tableName: "fd_object_scheme_field", indexName: "idx_object_scheme_field_organization_id") {
            column(name: "organization_id", type: "BIGINT UNSIGNED")
        }
    }

    changeSet(id: '2022-01-17-fd-object-scheme-field-drop-index', author: 'huaxin.deng@hand-china.com') {
        dropIndex(tableName: "fd_object_scheme_field", indexName: "pk_object_scheme_field_id")
    }

    changeSet(id: '2022-08-12-modify-object-scheme-field-column', author: 'kaiwen.li@zknow.com') {
        modifyDataType(tableName: "fd_object_scheme_field", columnName: "code", newDataType: "VARCHAR(64)")
        if (helper.isMysql()) {
            sql { " ALTER TABLE fd_object_scheme_field MODIFY COLUMN code varchar(64) NOT NULL COMMENT '字段code' " }
        }
    }
}