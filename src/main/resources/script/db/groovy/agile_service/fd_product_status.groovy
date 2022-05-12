package script.db.groovy.agile_service

databaseChangeLog(logicalFilePath: 'fd_product_status.groovy') {
    changeSet(id: '2022-05-07-fd-product-status', author: 'huaxin.deng@hand-china.com') {
        createTable(tableName: 'fd_product_status') {
            column(name: 'id', type: 'BIGINT UNSIGNED', autoIncrement: 'true', remarks: 'ID,主键') {
                constraints(primaryKey: true)
            }
            column(name: 'name', type: 'VARCHAR(64)', remarks: '名称') {
                constraints(nullable: false)
            }
            column(name: 'source', type: 'VARCHAR(30)', remarks: '来源') {
                constraints(nullable: false)
            }
            column(name: 'is_enabled', type: 'TINYINT UNSIGNED', defaultValue: "1", remarks: '是否启用') {
                constraints(nullable: false)
            }
            column(name: 'organization_id', type: 'BIGINT UNSIGNED', remarks: '组织id') {
                constraints(nullable: false)
            }

            column(name: "object_version_number", type: "BIGINT UNSIGNED", defaultValue: "1")
            column(name: "created_by", type: "BIGINT UNSIGNED", defaultValue: "0")
            column(name: "creation_date", type: "DATETIME", defaultValueComputed: "CURRENT_TIMESTAMP")
            column(name: "last_updated_by", type: "BIGINT UNSIGNED", defaultValue: "0")
            column(name: "last_update_date", type: "DATETIME", defaultValueComputed: "CURRENT_TIMESTAMP")
        }
        createIndex(tableName: "fd_product_status", indexName: "idx_organization_id") {
            column(name: "organization_id", type: "BIGINT UNSIGNED")
        }
        createIndex( tableName: 'fd_product_status', indexName: 'uk_name_organization_id', unique: true) {
            column(name: 'name')
            column(name: 'organization_id')
        }
    }
}