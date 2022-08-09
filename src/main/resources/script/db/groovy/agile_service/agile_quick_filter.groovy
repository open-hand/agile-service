package script.db.groovy.agile_service
databaseChangeLog(logicalFilePath:'agile_quick_filter.groovy') {
    changeSet(id: '2018-06-14-agile-quick-filter', author: 'fuqianghuang01@gmail.com') {
        createTable(tableName: "agile_quick_filter", remarks: '快速筛选表') {
            column(name: 'filter_id', type: 'BIGINT UNSIGNED', autoIncrement: true, remarks: 'id') {
                constraints(primaryKey: true)
            }
            column(name: 'project_id', type: 'BIGINT UNSIGNED', remarks: '项目id') {
                constraints(nullable: false)
            }
            column(name: 'name', type: 'VARCHAR(255)', remarks: '名称')
            column(name: 'sql_query', type: 'VARCHAR(1000)', remarks: '查询sql')
            column(name: 'express_query', type: 'VARCHAR(1000)', remarks: '查询表达式')
            column(name: 'is_child_included', type: 'TINYINT UNSIGNED', remarks: '是否包含子级')

            column(name: "object_version_number", type: "BIGINT UNSIGNED", defaultValue: "1")
            column(name: "created_by", type: "BIGINT UNSIGNED", defaultValue: "0")
            column(name: "creation_date", type: "DATETIME", defaultValueComputed: "CURRENT_TIMESTAMP")
            column(name: "last_updated_by", type: "BIGINT UNSIGNED", defaultValue: "0")
            column(name: "last_update_date", type: "DATETIME", defaultValueComputed: "CURRENT_TIMESTAMP")
        }
    }

    changeSet(id: '2018-06-22-agile-quick-filter-add-column', author: 'fuqianghuang01@gmail.com') {
        addColumn(tableName: 'agile_quick_filter') {
            column(name: 'description', type: 'VARCHAR(5000)', remarks: '描述')
        }
    }
    changeSet(id: '2018-06-26-agile-quick-filter-add-index', author: 'fuqianghuang01@gmail.com') {
        createIndex(tableName: "agile_quick_filter", indexName: "idx_project_id") {
            column(name: "project_id")
        }
    }
    changeSet(id: '2018-08-14-add-agile-quick-filter-column', author: 'dinghuang123@gmail.com') {
        addColumn(tableName: 'agile_quick_filter') {
            column(name: 'sequence', type: 'int', remarks: '排序字段', defaultValue: "0")
        }
        sql(stripComments: true, splitStatements: false, endDelimiter: ';') {
            "update agile_quick_filter set sequence = filter_id;"
        }
    }
}