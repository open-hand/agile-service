package script.db.groovy.agile_service
databaseChangeLog(logicalFilePath:'agile_configuration_rule_receiver.groovy') {
    changeSet(id: '2018-06-14-agile-configuration-rule', author: 'jiaxu.cui@hand-china.com') {
        createTable(tableName: "agile_configuration_rule_receiver") {
            column(name: 'id', type: 'BIGINT UNSIGNED', autoIncrement: true, remarks: 'rule receiver id') {
                constraints(primaryKey: true)
            }
            column(name: 'rule_id', type: 'BIGINT UNSIGNED', remarks: 'rule id') {
                constraints(nullable: false)
            }
            column(name: 'user_id', type: 'BIGINT UNSIGNED', remarks: 'rule id') {
                constraints(nullable: false)
            }
            column(name: 'user_type', type: 'VARCHAR(50)', remarks: 'user type') {
                constraints(nullable: false)
            }
            column(name: 'project_id', type: 'BIGINT UNSIGNED', remarks: 'project id') {
                constraints(nullable: false)
            }
            
            column(name: "object_version_number", type: "BIGINT UNSIGNED", defaultValue: "1")
            column(name: "created_by", type: "BIGINT UNSIGNED", defaultValue: "0")
            column(name: "creation_date", type: "DATETIME", defaultValueComputed: "CURRENT_TIMESTAMP")
            column(name: "last_updated_by", type: "BIGINT UNSIGNED", defaultValue: "0")
            column(name: "last_update_date", type: "DATETIME", defaultValueComputed: "CURRENT_TIMESTAMP")
        }
    }
}