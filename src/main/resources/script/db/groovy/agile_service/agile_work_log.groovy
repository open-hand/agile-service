package script.db.groovy.agile_service

databaseChangeLog(logicalFilePath: 'agile_work_log.groovyoovy') {
    changeSet(id: '2018-05-18-agile-work-log', author: 'fuqianghuang01@gmail.com') {
        createTable(tableName: "agile_work_log") {
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
}