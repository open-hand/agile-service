package script.db.groovy.agile_service

databaseChangeLog(logicalFilePath: 'agile_work_log.groovyoovy') {
    changeSet(id: '2018-05-18-agile-work-log', author: 'fuqianghuang01@gmail.com') {
        createTable(tableName: "agile_work_log", remarks: 'issue工时日志表') {
            column(name: 'log_id', type: 'BIGINT UNSIGNED', autoIncrement: true, remarks: '日志id') {
                constraints(primaryKey: true)
            }
            column(name: 'work_time', type: 'DECIMAL', remarks: '工作时间') {
                constraints(nullable: false)
            }
            column(name: 'start_date', type: 'DATETIME', remarks: '开始日期') {
                constraints(nullable: false)
            }
            column(name: 'description', type: 'VARCHAR(5000)', remarks: '描述')
            column(name: 'issue_id', type: 'BIGINT UNSIGNED', remarks: '问题id') {
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
    changeSet(id: '2019-02-26-modify-data-type', author: 'shinan.chenX@gmail.com') {
        modifyDataType(tableName: 'agile_work_log', columnName: 'work_time', newDataType: "DECIMAL(10,1)")
    }
    changeSet(id: '2019-05-06-agile-work-log-add-index', author: 'shinan.chenX@gmail.com') {
        createIndex(tableName: "agile_work_log", indexName: "idx_issueid_projectid_creationdate") {
            column(name: 'project_id')
            column(name: 'issue_id')
            column(name: 'creation_date')
        }
    }

    changeSet(id: '2022-10-12-agile-work-log-description-type', author: 'gaokuo.dai@zknow.com') {
        modifyDataType (tableName: "agile_work_log", columnName: "description", newDataType: "text")
        //mysql在修改列类型时候会清空非空约束、备注和默认值
        if (helper.isMysql()) {
            renameColumn (tableName: "agile_work_log", oldColumnName: "description", newColumnName: "description", columnDataType: "text", remarks: "描述")
        }
        //修复changeSet'2019-02-26-modify-data-type'遗留的BUG
        //mysql在修改列类型时候会清空非空约束、备注和默认值
        if (helper.isMysql()) {
            renameColumn (tableName: "agile_work_log", oldColumnName: "work_time", newColumnName: "work_time", columnDataType: "DECIMAL(10,1)", remarks: "工作时间")
            addNotNullConstraint (tableName: "agile_work_log", columnName: "work_time", columnDataType: "DECIMAL(10,1)")
        }
    }
}
