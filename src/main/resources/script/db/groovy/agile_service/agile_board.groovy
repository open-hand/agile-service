package script.db.groovy.agile_service
databaseChangeLog(logicalFilePath:'agile_board.groovyoovy') {
    changeSet(id: '2018-05-14-agile-board', author: 'fuqianghuang01@gmail.com') {
        createTable(tableName: "agile_board", remarks: '敏捷看板表') {
            column(name: 'board_id', type: 'BIGINT UNSIGNED', autoIncrement: true, remarks: '看板id') {
                constraints(primaryKey: true)
            }
            column(name: 'name', type: 'VARCHAR(255)', remarks: '名称') {
                constraints(nullable: false)
            }
            column(name: 'project_id', type: 'BIGINT UNSIGNED', remarks: '项目id') {
                constraints(nullable: false)
            }
            column(name: 'administrator_id', type: 'BIGINT UNSIGNED', remarks: '管理员id')
            column(name: 'column_constraint', type: 'VARCHAR(30)', remarks: '列约束') {
                constraints(nullable: false)
            }
            column(name: 'is_day_in_column', type: 'TINYINT UNSIGNED', remarks: '是否为列中的天') {
                constraints(nullable: false)
            }
            column(name: 'swimlane_based_code', type: 'VARCHAR(30)', remarks: '基于code的泳道') {
                constraints(nullable: false)
            }
            column(name: 'estimation_statistic', type: 'VARCHAR(30)', remarks: '预估统计') {
                constraints(nullable: false)
            }

            column(name: "object_version_number", type: "BIGINT UNSIGNED", defaultValue: "1")
            column(name: "created_by", type: "BIGINT UNSIGNED", defaultValue: "0")
            column(name: "creation_date", type: "DATETIME", defaultValueComputed: "CURRENT_TIMESTAMP")
            column(name: "last_updated_by", type: "BIGINT UNSIGNED", defaultValue: "0")
            column(name: "last_update_date", type: "DATETIME", defaultValueComputed: "CURRENT_TIMESTAMP")
        }
    }

    changeSet(id: '2018-07-02-agile-board-update-data', author: 'fuqianghuang01@gmail.com') {
        sql(stripComments: true, splitStatements: true, endDelimiter: ';') {
            "UPDATE agile_board SET swimlane_based_code = 'parent_child' WHERE swimlane_based_code = 'story';"
        }
    }

    changeSet(author: 'ztxemail@163.com',id: '2021-03-23-agile-board-add-column'){
        addColumn(tableName: 'agile_board') {
            column(name: 'organization_id', type: 'BIGINT UNSIGNED', remarks: '组织Id', defaultValue: "0")
        }
    }

    changeSet(author: 'ztxemail@163.com',id: '2022-01-11-agile-board-add-column'){
        addColumn(tableName: 'agile_board') {
            column(name: 'type', type: 'VARCHAR(30)', remarks: '看板类型(agile/program)')
        }
    }
}