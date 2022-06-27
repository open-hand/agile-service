package script.db.groovy.agile_service

databaseChangeLog(logicalFilePath: 'agile_issue_product_rel.groovy') {
    changeSet(id: '2022-04-27-agile-issue-product-rel', author: 'huaxin.deng@hand-china.com') {
        createTable(tableName: "agile_issue_product_rel", remarks: "工作项产品关联表") {
            column(name: 'id', type: 'BIGINT UNSIGNED', autoIncrement: true, remarks: 'id') {
                constraints(primaryKey: true)
            }
            column(name: 'issue_id', type: 'BIGINT UNSIGNED', remarks: '问题id') {
                constraints(nullable: false)
            }
            column(name: 'product_id', type: 'BIGINT UNSIGNED', remarks: '产品id') {
                constraints(nullable: false)
            }
            column(name: 'project_id', type: 'BIGINT UNSIGNED', remarks: '项目id') {
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

        createIndex(indexName: "idx_issue_id", tableName: "agile_issue_product_rel") {
            column(name: "issue_id")
        }
        createIndex(indexName: "idx_project_id", tableName: "agile_issue_product_rel") {
            column(name: "project_id")
        }
        createIndex(indexName: "idx_product_id", tableName: "agile_issue_product_rel") {
            column(name: "product_id")
        }
        createIndex(indexName: "uk_issue_product", tableName: "agile_issue_product_rel", unique: true) {
            column(name: "issue_id")
            column(name: "product_id")
        }
    }

}