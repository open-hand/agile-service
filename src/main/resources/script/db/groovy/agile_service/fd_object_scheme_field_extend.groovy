package script.db.groovy.agile_service

databaseChangeLog(logicalFilePath: 'fd_object_scheme_field_extend.groovy') {
    changeSet(id: '2020-08-09-create-table-object-scheme-field-extend', author: 'superlee') {
        createTable(tableName: "fd_object_scheme_field_extend", remarks: '字段扩展表') {
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

    changeSet(id: '2020-09-14-fd-object-scheme-field-extend-add-index', author: 'jiaxu.cui@hand-china.com') {
        createIndex(tableName: "fd_object_scheme_field_extend", indexName: "idx_issue_type_id") {
            column(name: "issue_type_id")
        }
        createIndex(tableName: "fd_object_scheme_field_extend", indexName: "idx_issue_type") {
            column(name: "issue_type")
        }
        createIndex(tableName: "fd_object_scheme_field_extend", indexName: "idx_organization_id") {
            column(name: "organization_id")
        }
        createIndex(tableName: "fd_object_scheme_field_extend", indexName: "idx_project_id") {
            column(name: "project_id")
        }
        createIndex(tableName: "fd_object_scheme_field_extend", indexName: "idx_field_id") {
            column(name: "field_id")
        }
    }

    changeSet(id: '2020-12-25-fd-object-scheme-field-extend-add-column', author: 'kaiwen.li@hand-china.com') {
        addColumn(tableName: 'fd_object_scheme_field_extend') {
            column(name: 'default_value', type: 'VARCHAR(255)', remarks: '默认值')
        }
        sql(stripComments: true, splitStatements: false, endDelimiter: ';') {
            "UPDATE fd_object_scheme_field_extend t1 " +
                    "JOIN ( SELECT t2.id, t2.default_value FROM fd_object_scheme_field t2 WHERE t2.default_value IS NOT NULL) t3 " +
                    "ON t1.field_id = t3.id " +
                    "SET t1.default_value = t3.default_value;"
        }
    }

    changeSet(id: '2021-01-05-fd-object-scheme-field-extend-add-column', author: 'huaxin.deng@hand-china.com') {
        addColumn(tableName: 'fd_object_scheme_field_extend') {
            column(name: 'extra_config', type: 'TINYINT UNSIGNED(1)', remarks: '额外配置（是否当前时间/是否包括小数）')
        }
        sql(stripComments: true, splitStatements: false, endDelimiter: ';') {
            "UPDATE fd_object_scheme_field_extend t1 " +
                    "JOIN ( SELECT t2.id, t2.extra_config FROM fd_object_scheme_field t2 WHERE t2.extra_config IS NOT NULL) t3 " +
                    "ON t1.field_id = t3.id " +
                    "SET t1.extra_config = t3.extra_config;"
        }
    }
}