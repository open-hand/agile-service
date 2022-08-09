package script.db.groovy.agile_service


databaseChangeLog(logicalFilePath: 'fd_organization_config.groovy') {
    changeSet(id: '2021-03-22-create-organization-config', author: 'ztxemail@163.com') {
        createTable(tableName: 'fd_organization_config', remarks: '组织方案配置表') {
            column(name: 'id', type: 'BIGINT UNSIGNED', autoIncrement: 'true', remarks: '主键') {
                constraints(primaryKey: 'true')
            }
            column(name: 'organization_id', type: 'BIGINT UNSIGNED', remarks: '组织id') {
                constraints(nullable: 'false')
            }
            column(name: 'scheme_id', type: 'BIGINT UNSIGNED', remarks: '方案id') {
                constraints(nullable: 'false')
            }
            column(name: 'scheme_type', type: 'VARCHAR(30)', remarks: '方案类型') {
                constraints(nullable: 'true')
            }
            column(name: 'apply_type', type: 'VARCHAR(30)', remarks: '应用类型') {
                constraints(nullable: 'true')
            }


            column(name: "object_version_number", type: "BIGINT UNSIGNED", defaultValue: "1")
            column(name: "created_by", type: "BIGINT UNSIGNED", defaultValue: "0")
            column(name: "creation_date", type: "DATETIME", defaultValueComputed: "CURRENT_TIMESTAMP")
            column(name: "last_updated_by", type: "BIGINT UNSIGNED", defaultValue: "0")
            column(name: "last_update_date", type: "DATETIME", defaultValueComputed: "CURRENT_TIMESTAMP")
        }
    }
}