package script.db.groovy.agile_service
databaseChangeLog(logicalFilePath:'agile_configuration_rule.groovy') {
    changeSet(id: '2020-09-24-agile-configuration-rule', author: 'jiaxu.cui@hand-china.com') {
        createTable(tableName: "agile_configuration_rule") {
            column(name: 'id', type: 'BIGINT UNSIGNED', autoIncrement: true, remarks: 'rule id') {
                constraints(primaryKey: true)
            }
            column(name: 'project_id', type: 'BIGINT UNSIGNED', remarks: 'project id') {
                constraints(nullable: false)
            }
            column(name: 'sql_query', type: 'VARCHAR(1000)', remarks: 'long query')
            column(name: 'express_query', type: 'VARCHAR(1000)', remarks: 'express query')
            column(name: 'express_format', type: 'VARCHAR(5000)', remarks: '表达式数据')

            column(name: "object_version_number", type: "BIGINT UNSIGNED", defaultValue: "1")
            column(name: "created_by", type: "BIGINT UNSIGNED", defaultValue: "0")
            column(name: "creation_date", type: "DATETIME", defaultValueComputed: "CURRENT_TIMESTAMP")
            column(name: "last_updated_by", type: "BIGINT UNSIGNED", defaultValue: "0")
            column(name: "last_update_date", type: "DATETIME", defaultValueComputed: "CURRENT_TIMESTAMP")
        }
    }

    changeSet(id: '2020-10-21-add-column', author: 'jiaxu.cui@hand-china.com') {
        addColumn(tableName: 'agile_configuration_rule') {
            column(name: 'name', type: 'varchar(50)', remarks: '名称')
            column(name: 'type_code', type: 'VARCHAR(50)', remarks: '类型code')
            column(name: 'source', type: 'VARCHAR(50)', remarks: '来源')
            column(name: 'is_enabled', type: 'TINYINT UNSIGNED', remarks: 'enabled flag', defaultValue: "1") {
                constraints(nullable: false)
            }
        }
    }
}