package script.db.groovy.agile_service

databaseChangeLog(logicalFilePath: 'script/db/agile_product_version.groovy') {
    changeSet(id: '2018-05-14-agile-product-version', author: 'jian_zhang02@163.com') {
        createTable(tableName: "agile_product_version", remarks: '产品版本表') {
            column(name: 'version_id', type: 'BIGINT UNSIGNED', autoIncrement: true, remarks: '版本id') {
                constraints(primaryKey: true)
            }
            column(name: 'name', type: 'VARCHAR(255)', remarks: '名称') {
                constraints(nullable: false)
            }
            column(name: 'description', type: 'VARCHAR(5000)', remarks: '描述')
            column(name: 'start_date', type: 'DATETIME', remarks: '开始日期')
            column(name: 'release_date', type: 'DATETIME', remarks: '发布日期')
            column(name: 'status_code', type: 'VARCHAR(255)', remarks: '状态id') {
                constraints(nullable: false)
            }
            column(name: 'process', type: 'DECIMAL', remarks: '进度', defaultValue: "0")

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

    changeSet(id: '2018-05-24-add-column', author: 'jian_zhang02@163.com') {
        addColumn(tableName: 'agile_product_version') {
            column(name: 'archived_date', type: 'DATETIME', remarks: '归档日期')
        }
    }

    changeSet(id: '2018-06-06-drop-column', author: 'jian_zhang02@163.com') {
        dropColumn(tableName: 'agile_product_version') {
            column(name: 'process')
        }
    }

    changeSet(id: '2018-06-20-add-column', author: 'jian_zhang02@163.com') {
        addColumn(tableName: 'agile_product_version') {
            column(name: 'old_status_code', type: 'VARCHAR(255)', remarks: '旧状态编码')
        }
    }

    changeSet(id: '2018-07-26-add-column', author: 'dinghuang123@gmail.com') {
        addColumn(tableName: 'agile_product_version') {
            column(name: 'sequence', type: 'int', remarks: '排序字段', defaultValue: "0")
        }
        sql(stripComments: true, splitStatements: false, endDelimiter: ';') {
            "update agile_product_version set sequence = version_id;"
        }
    }

    changeSet(id: '2018-12-03-version-add-column', author: 'fuqianghuang01@gmail.com') {
        addColumn(tableName: 'agile_product_version') {
            column(name: 'expect_release_date', type: 'DATETIME', remarks: '期望发布日期')
        }
    }
}