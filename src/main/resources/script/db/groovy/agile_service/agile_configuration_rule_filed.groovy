package script.db.groovy.agile_service
databaseChangeLog(logicalFilePath:'agile_configuration_rule_filed.groovy'){
    changeSet(id: '2020-09-24-agile-configuration-rule-filed', author: 'jiaxu.cui@hand-china.com') {
        createTable(tableName: "agile_configuration_rule_filed") {
            column(name: 'field_code', type: 'VARCHAR(255)',  remarks: 'field code') {
                constraints(primaryKey: true)
            }
            column(name: 'type', type: 'VARCHAR(255)', remarks: 'type')
            column(name: 'name', type: 'VARCHAR(255)', remarks: 'name')
            column(name: 'field', type: 'VARCHAR(255)', remarks: 'field')

            column(name: "object_version_number", type: "BIGINT UNSIGNED", defaultValue: "1")
            column(name: "created_by", type: "BIGINT UNSIGNED", defaultValue: "0")
            column(name: "creation_date", type: "DATETIME", defaultValueComputed: "CURRENT_TIMESTAMP")
            column(name: "last_updated_by", type: "BIGINT UNSIGNED", defaultValue: "0")
            column(name: "last_update_date", type: "DATETIME", defaultValueComputed: "CURRENT_TIMESTAMP")
        }
    }
}