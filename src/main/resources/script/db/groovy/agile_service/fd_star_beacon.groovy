package script.db.groovy.agile_service

databaseChangeLog(logicalFilePath: 'fd_star_beacon.groovy') {
    changeSet(id: '2020-11-09-fd-star-beacon', author: 'huaxin.deng@hand-china.com') {
        createTable(tableName: "fd_star_beacon", remarks: '星标配置表') {
            column(name: 'id', type: 'BIGINT UNSIGNED', autoIncrement: true, remarks: 'id') {
                constraints(primaryKey: true)
            }
            column(name: 'type', type: 'VARCHAR(255)', remarks: '类型') {
                constraints(nullable: false)
            }
            column(name: 'instance_id', type: 'BIGINT UNSIGNED', remarks: 'instance id包括issueId和backlogId') {
                constraints(nullable: false)
            }
            column(name: 'project_id', type: 'BIGINT UNSIGNED', remarks: '项目id') {
                constraints(nullable: false)
            }
            column(name: 'organization_id', type: 'BIGINT UNSIGNED', remarks: '组织id') {
                constraints(nullable: false)
            }
            column(name: 'user_id', type: 'BIGINT UNSIGNED', remarks: '用户id') {
                constraints(nullable: false)
            }

            column(name: "object_version_number", type: "BIGINT UNSIGNED", defaultValue: "1")
            column(name: "created_by", type: "BIGINT UNSIGNED", defaultValue: "0")
            column(name: "creation_date", type: "DATETIME", defaultValueComputed: "CURRENT_TIMESTAMP")
            column(name: "last_updated_by", type: "BIGINT UNSIGNED", defaultValue: "0")
            column(name: "last_update_date", type: "DATETIME", defaultValueComputed: "CURRENT_TIMESTAMP")
        }
    }

    changeSet(id: '2020-11-16-fd-star-beacon-add-index', author: 'huaxin.deng@hand-china.com') {
        createIndex(indexName: "idx_type", tableName: "fd_star_beacon") {
            column(name: "type")
        }
        createIndex(indexName: "idx_instance_id", tableName: "fd_star_beacon") {
            column(name: "instance_id")
        }
        createIndex(indexName: "idx_project_id", tableName: "fd_star_beacon") {
            column(name: "project_id")
        }
        createIndex(indexName: "idx_user_id", tableName: "fd_star_beacon") {
            column(name: "user_id")
        }
    }

}