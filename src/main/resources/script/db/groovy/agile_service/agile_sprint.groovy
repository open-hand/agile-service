package script.db.groovy.agile_service

databaseChangeLog(logicalFilePath: 'script/db/agile_sprint.groovy') {
    changeSet(id: '2018-05-14-agile-sprint', author: 'jian_zhang02@163.com') {
        createTable(tableName: "agile_sprint") {
            column(name: 'sprint_id', type: 'BIGINT UNSIGNED', autoIncrement: true, remarks: '冲刺id') {
                constraints(primaryKey: true)
            }
            column(name: 'sprint_name', type: 'VARCHAR(255)', remarks: '冲刺名称') {
                constraints(nullable: false)
            }
            column(name: 'sprint_goal', type: 'VARCHAR(255)', remarks: '冲刺目标')
            column(name: 'duration', type: 'SMALLINT UNSIGNED', remarks: '间隔') {
                constraints(nullable: false)
            }
            column(name: 'start_date', type: 'DATETIME', remarks: '开始日期')
            column(name: 'end_date', type: 'DATETIME', remarks: '结束日期')
            column(name: 'actual_end_date', type: 'DATETIME', remarks: '实际结束日期')
            column(name: 'status_code', type: 'VARCHAR(10)', remarks: '状态编码') {
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
    changeSet(id: '2018-05-28-modify-column', author: 'jian_zhang02@163.com') {
        modifyDataType(tableName: 'agile_sprint', columnName: 'status_code', newDataType: "VARCHAR(255)")
    }

    changeSet(id: '2018-05-31-drop-not-null-constraint', author: 'jian_zhang02@163.com') {
        dropNotNullConstraint(schemaName: '', tableName: 'agile_sprint', columnName: 'duration', columnDataType: 'SMALLINT UNSIGNED')
    }

    changeSet(id: '2018-06-06-drop-column', author: 'jian_zhang02@163.com') {
        dropColumn(tableName: 'agile_sprint') {
            column(name: 'duration')
        }
    }

    changeSet(id: '2019-03-13-add-column-pi-id', author: 'fuqianghuang01@gmail.com') {
        addColumn(tableName: 'agile_sprint') {
            column(name: 'pi_id', type: 'BIGINT UNSIGNED', remarks: 'pi id')
        }
    }

    changeSet(id: '2019-05-27-add-index', author: 'shinan.chenX@gmail.com') {
        createIndex(tableName: "agile_sprint", indexName: "idx_sprint_project_id") {
            column(name: "project_id", type: "BIGINT UNSIGNED")
        }
    }
}