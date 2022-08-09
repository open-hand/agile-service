package script.db.groovy.agile_service

databaseChangeLog(logicalFilePath: 'fd_status_machine_transform.groovy') {
    changeSet(author: 'ztxemail@163.com', id: '2020-08-17-status-machine-transform') {
        createTable(tableName: 'fd_status_machine_transform', remarks: '状态机转换表') {
            column(name: 'id', type: 'BIGINT UNSIGNED', autoIncrement: true, remarks: 'ID,主键') {
                constraints(primaryKey: true)
            }
            column(name: 'name', type: 'VARCHAR(64)', remarks: '名称')
            column(name: 'description', type: 'VARCHAR(255)', remarks: '描述')
            column(name: 'state_machine_id', type: 'BIGINT UNSIGNED', remarks: '状态机id') {
                constraints(nullable: false)
            }
            column(name: 'start_node_id', type: 'BIGINT UNSIGNED', remarks: '起始节点id') {
                constraints(nullable: false)
            }
            column(name: 'end_node_id', type: 'BIGINT UNSIGNED', remarks: '结束节点id') {
                constraints(nullable: false)
            }
            column(name: 'url', type: 'VARCHAR(255)', remarks: '页面')
            column(name: 'type', type: 'VARCHAR(30)', remarks: '类型') {
                constraints(nullable: false)
            }
            column(name: 'style', type: 'clob', remarks: '样式')
            column(name: 'condition_strategy', type: 'VARCHAR(20)', remarks: '条件策略', defaultValue: "condition_all")
            column(name: 'organization_id', type: 'BIGINT UNSIGNED', remarks: '组织id') {
                constraints(nullable: false)
            }

            column(name: "object_version_number", type: "BIGINT UNSIGNED", defaultValue: "1")
            column(name: "created_by", type: "BIGINT UNSIGNED", defaultValue: "0")
            column(name: "creation_date", type: "DATETIME", defaultValueComputed: "CURRENT_TIMESTAMP")
            column(name: "last_updated_by", type: "BIGINT UNSIGNED", defaultValue: "0")
            column(name: "last_update_date", type: "DATETIME", defaultValueComputed: "CURRENT_TIMESTAMP")
        }
        createIndex(tableName: "fd_status_machine_transform", indexName: "status_machine_transform_n1") {
            column(name: "state_machine_id", type: "BIGINT UNSIGNED")
        }
        createIndex(tableName: "fd_status_machine_transform", indexName: "status_machine_transform_n2") {
            column(name: "type", type: "VARCHAR(30)")
        }
    }
}