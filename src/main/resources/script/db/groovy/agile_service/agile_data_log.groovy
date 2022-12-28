package script.db.groovy.agile_service
databaseChangeLog(logicalFilePath:'agile_data_log.groovy') {
    changeSet(id: '2018-06-20-agile-data-log', author: 'fuqianghuang01@gmail.com') {
        createTable(tableName: "agile_data_log", remarks: 'issue操作记录表（字段值变更记录）') {
            column(name: 'log_id', type: 'BIGINT UNSIGNED', autoIncrement: true, remarks: '日志id') {
                constraints(primaryKey: true)
            }
            column(name: 'project_id', type: 'BIGINT UNSIGNED', remarks: '项目id') {
                constraints(nullable: false)
            }
            column(name: 'field', type: 'VARCHAR(255)', remarks: '字段')
            column(name: 'old_value', type: 'text', remarks: '旧值id')
            column(name: 'old_string', type: 'text', remarks: '旧值')
            column(name: 'new_value', type: 'text', remarks: '新值id')
            column(name: 'new_string', type: 'text', remarks: '新值')
            column(name: 'issue_id', type: 'BIGINT UNSIGNED', remarks: '问题id')

            column(name: "object_version_number", type: "BIGINT UNSIGNED", defaultValue: "1")
            column(name: "created_by", type: "BIGINT UNSIGNED", defaultValue: "0")
            column(name: "creation_date", type: "DATETIME", defaultValueComputed: "CURRENT_TIMESTAMP")
            column(name: "last_updated_by", type: "BIGINT UNSIGNED", defaultValue: "0")
            column(name: "last_update_date", type: "DATETIME", defaultValueComputed: "CURRENT_TIMESTAMP")
        }
    }

    changeSet(id: '2018-06-26-agile-data-log-add-index', author: 'fuqianghuang01@gmail.com') {
        createIndex(tableName: "agile_data_log", indexName: "idx_project_id") {
            column(name: "project_id")
        }
        createIndex(tableName: "agile_data_log", indexName: "idx_issue_id") {
            column(name: "issue_id")
        }
        createIndex(tableName: "agile_data_log", indexName: "idx_field") {
            column(name: "field")
        }
    }

    changeSet(id: '2018-08-07-agile-data-log-add-index', author: 'dinghuang123@gmail.com') {
        createIndex(indexName: "idx_old_value", tableName: "agile_data_log") {
            column(name: "old_value(10)")
        }
        createIndex(indexName: "idx_new_value", tableName: "agile_data_log") {
            column(name: "new_value(10)")
        }
        createIndex(indexName: "idx_old_string", tableName: "agile_data_log") {
            column(name: "old_string(10)")
        }
        createIndex(indexName: "idx_new_string", tableName: "agile_data_log") {
            column(name: "new_string(10)")
        }
        createIndex(indexName: "idx_creation_date", tableName: "agile_data_log") {
            column(name: "creation_date")
        }
    }

    changeSet(id: '2018-08-20-modify-data', author: 'dinghuang123@gmail.com') {
        sql(stripComments: true, splitStatements: false, endDelimiter: ';') {
            "delete FROM agile_data_log WHERE field = 'Sprint' and creation_date > '2018-08-20 00:00:00'" +
                    " and log_id NOT IN ( SELECT temp.log_id FROM ( SELECT min(log_id) as log_id FROM agile_data_log adl where" +
                    " field = 'Sprint' and creation_date > '2018-08-20 00:00:00' GROUP BY adl.field,adl.old_value,adl.new_value," +
                    "adl.old_string,adl.new_string,adl.issue_id) AS temp);"
        }
    }

    changeSet(id: '2019-05-06-agile-data-log-add-index', author: 'shinan.chenX@gmail.com') {
        createIndex(tableName: "agile_data_log", indexName: "idx_projectid_field") {
            column(name: 'project_id')
            column(name: 'field')
        }
    }

    changeSet(id: '2022-01-17-agile-data-log-drop-index', author: 'huaxin.deng@hand-china.com') {
        dropIndex(tableName: "agile_data_log", indexName: "idx_project_id")
    }

    changeSet(id: '2022-12-27-agile-data-log-modify-data-type-old/new_string', author: 'gaokuo.dai@zknow.com') {
        modifyDataType (tableName: "agile_data_log", columnName: "old_string", newDataType: "longtext")
        if (helper.isMysql()) {
            setColumnRemarks (tableName: "agile_data_log", columnName: "old_string", remarks: "旧值")
        }
        modifyDataType (tableName: "agile_data_log", columnName: "new_string", newDataType: "longtext")
        if (helper.isMysql()) {
            setColumnRemarks (tableName: "agile_data_log", columnName: "new_string", remarks: "新值")
        }
    }
}
