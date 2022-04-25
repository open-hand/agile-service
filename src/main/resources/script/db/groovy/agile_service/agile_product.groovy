package script.db.groovy.agile_service

databaseChangeLog(logicalFilePath: 'agile_product.groovy') {
    changeSet(id: '2022-04-25-agile-product', author: 'huaxin.deng@hand-china.com') {
        createTable(tableName: "agile_product", remarks: '产品表') {
            column(name: 'id', type: 'BIGINT UNSIGNED', autoIncrement: true, remarks: 'id') {
                constraints(primaryKey: true)
            }
            column(name: 'organization_id', type: 'BIGINT UNSIGNED', remarks: '组织id') {
                constraints(nullable: false)
            }
            column(name: 'name', type: 'VARCHAR(255)', remarks: '产品名称') {
                constraints(nullable: false)
            }
            column(name: 'code', type: 'VARCHAR(255)', remarks: '产品编码') {
                constraints(nullable: false)
            }
            column(name: 'description', type: 'text', remarks: '描述')
            column(name: 'product_owner', type: 'BIGINT UNSIGNED', remarks: '产品负责人')
            column(name: 'start_date', type: 'DATETIME', remarks: '开始时间')
            column(name: 'end_date', type: 'DATETIME', remarks: '结束时间')
            column(name: 'is_enabled', type: 'TINYINT UNSIGNED', defaultValue: "1", remarks: '是否启用') {
                constraints(nullable: false)
            }

            column(name: "object_version_number", type: "BIGINT UNSIGNED", defaultValue: "1")
            column(name: "created_by", type: "BIGINT UNSIGNED", defaultValue: "0")
            column(name: "creation_date", type: "DATETIME", defaultValueComputed: "CURRENT_TIMESTAMP")
            column(name: "last_updated_by", type: "BIGINT UNSIGNED", defaultValue: "0")
            column(name: "last_update_date", type: "DATETIME", defaultValueComputed: "CURRENT_TIMESTAMP")
        }

        createIndex(tableName: "agile_product", indexName: "idx_organization_id") {
            column(name: "organization_id")
        }
        createIndex(tableName: "agile_product", indexName: "uk_organization_name", unique: true) {
            column(name: "organization_id")
            column(name: "name")
        }
        createIndex(tableName: "agile_product", indexName: "uk_organization_code", unique: true) {
            column(name: "organization_id")
            column(name: "code")
        }
    }
}
