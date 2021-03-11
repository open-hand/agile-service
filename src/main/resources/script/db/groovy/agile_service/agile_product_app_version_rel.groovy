package script.db.groovy.agile_service

databaseChangeLog(logicalFilePath: 'script/db/agile_product_app_version_rel.groovy') {
    changeSet(id: '2021-03-09-agile-product-app-version-rel', author: 'kaiwen.li@hand-china.com') {
        createTable(tableName: "agile_product_app_version_rel", remarks: '产品版本和应用版本表关系表') {
            column(name: 'id', type: 'BIGINT UNSIGNED', autoIncrement: true, remarks: 'id') {
                constraints(primaryKey: true)
            }
            column(name: 'app_version_id', type: 'BIGINT UNSIGNED', remarks: 'app版本id') {
                constraints(nullable: false)
            }
            column(name: 'product_version_id', type: 'BIGINT UNSIGNED', remarks: '产品版本id') {
                constraints(nullable: false)
            }
            column(name: 'project_id', type: 'BIGINT UNSIGNED', remarks: 'project id') {
                constraints(nullable: false)
            }
            column(name: 'organization_id', type: 'BIGINT UNSIGNED', remarks: 'organization id') {
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