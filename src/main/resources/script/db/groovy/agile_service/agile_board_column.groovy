package script.db.groovy.agile_service
databaseChangeLog(logicalFilePath:'agile_board_column.groovyoovy') {
    changeSet(id: '2018-05-14-agile-board-column', author: 'fuqianghuang01@gmail.com') {
        createTable(tableName: "agile_board_column") {
            column(name: 'column_id', type: 'BIGINT UNSIGNED', autoIncrement: true, remarks: '列id') {
                constraints(primaryKey: true)
            }
            column(name: 'status_code', type: 'VARCHAR(30)', remarks: '状态编码') {
                constraints(nullable: false)
            }
            column(name: 'name', type: 'VARCHAR(255)', remarks: '名称') {
                constraints(nullable: false)
            }
            column(name: 'board_id', type: 'BIGINT UNSIGNED', remarks: '看板id') {
                constraints(nullable: false)
            }
            column(name: 'min_num', type: 'BIGINT UNSIGNED', remarks: '最小值')
            column(name: 'max_num', type: 'BIGINT UNSIGNED', remarks: '最大值')
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
    changeSet(id: '2018-05-24-agile-board-column-add-column', author: 'fuqianghuang01@gmail.com') {
        addColumn(tableName: 'agile_board_column') {
            column(name: 'sequence', type: 'INTEGER UNSIGNED', remarks: '列排序') {
                constraints(nullable: false)
            }
        }
    }
    changeSet(id:  '2018-05-24-agile-board-column-rename-column', author: 'fuqianghuang01@gmail.com') {
        renameColumn(columnDataType: 'VARCHAR(30)', newColumnName: 'category_code', oldColumnName: 'status_code', remarks: 'status code',tableName: 'agile_board_column')
    }
    changeSet(id: '2018-06-05-agile-board-column-add-column', author: 'fuqianghuang01@gmail.com') {
        addColumn(tableName: 'agile_board_column') {
            column(name: 'color', type: 'VARCHAR(20)', remarks: '颜色') {
                constraints(nullable: false)
            }
        }
    }
    changeSet(id: '2018-06-06-agile-board-column-add-index', author: 'fuqianghuang01@gmail.com') {
        createIndex(tableName: "agile_board_column", indexName: "idx_board_id ") {
            column(name: "board_id")
        }
    }
    changeSet(id: '2018-06-05-agile-board-column-rename-color', author: 'fuqianghuang01@gmail.com') {
        renameColumn(columnDataType: 'VARCHAR(50)', newColumnName: 'color_code', oldColumnName: 'color', remarks: 'color code',tableName: 'agile_board_column')
    }

    changeSet(author: 'ztxemail@163.com',id: '2021-03-23-agile-board-column-add-column'){
        addColumn(tableName: 'agile_board_column') {
            column(name: 'organization_id', type: 'BIGINT UNSIGNED', remarks: '组织Id', defaultValue: "0")
        }
    }
}