package script.db.groovy.agile_service
databaseChangeLog(logicalFilePath:'agile_board_column_status_rel.groovyoovy') {
    changeSet(id: '2018-05-14-agile-board-column-status-rel', author: 'fuqianghuang01@gmail.com') {
        createTable(tableName: "agile_board_column_status_rel") {
            column(name: 'position', type: 'INTEGER UNSIGNED', remarks: '位置') {
                constraints(nullable: false)
            }
            column(name: 'status_id', type: 'BIGINT UNSIGNED', remarks: '状态id') {
                constraints(nullable: false)
            }
            column(name: 'column_id', type: 'BIGINT UNSIGNED', remarks: '列id') {
                constraints(nullable: false)
            }
            column(name: 'project_id', type: 'BIGINT UNSIGNED', remarks: '项目id') {
                constraints(nullable: false)
            }

            column(name: "object_version_number", type: "BIGINT UNSIGNED", defaultValue: "1")
            column(name: "created_by", type: "BIGINT UNSIGNED", defaultValue: "0")
            column(name: "creation_date", type: "DATETIME", defaultValueComputed: "CURRENT_TIMESTAMP")
            column(name: "last_updated_by", type: "BIGINT UNSIGNED", defaultValue: "0")
            column(name: "last_update_date", type: "DATETIME", defaultValueComputed: "CURRENT_TIMESTAMP")
        }
    }

    changeSet(id: '2018-08-07-agile-board-column-status-rel-add-index', author: 'dinghuang123@gmail.com') {
        createIndex(indexName: "idx_status_id", tableName: "agile_board_column_status_rel") {
            column(name: "status_id")
        }
    }

    changeSet(id: '2020-05-13-agile-board-column-status-rel-add-column', author: 'fuqianghuang01@gmail.com') {
        addColumn(tableName: 'agile_board_column_status_rel') {
            column(name: 'id', type: 'BIGINT UNSIGNED', autoIncrement: true, remarks: 'id') {
                constraints(primaryKey: true)
            }
        }
    }

    changeSet(author: 'ztxemail@163.com',id: '2021-03-23-agile-board-column-status-rel-add-column'){
        addColumn(tableName: 'agile_board_column_status_rel') {
            column(name: 'organization_id', type: 'BIGINT UNSIGNED', remarks: '组织Id', defaultValue: "0")
        }
    }
}