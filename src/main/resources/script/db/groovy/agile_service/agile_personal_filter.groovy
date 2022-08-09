package script.db.groovy.agile_service
databaseChangeLog(logicalFilePath:'script/db/agile_personal_filter.groovy') {
    changeSet(id: '2019-02-25-agile-personal-filter', author: 'shinan.chenX@gmail.com') {
        createTable(tableName: "agile_personal_filter", remarks: '个人筛选表') {
            column(name: 'filter_id', type: 'BIGINT UNSIGNED', autoIncrement: true, remarks: '主键id') {
                constraints(primaryKey: true)
            }
            column(name: 'project_id', type: 'BIGINT UNSIGNED', remarks: '项目id') {
                constraints(nullable: false)
            }
            column(name: 'name', type: 'VARCHAR(255)', remarks: '名称') {
                constraints(nullable: false)
            }
            column(name: 'user_id', type: 'BIGINT UNSIGNED', remarks: '用户id') {
                constraints(nullable: false)
            }
            column(name: 'filter_json', type: 'TEXT', remarks: '筛选条件的json') {
                constraints(nullable: false)
            }

            column(name: "object_version_number", type: "BIGINT UNSIGNED", defaultValue: "1")
            column(name: "created_by", type: "BIGINT UNSIGNED", defaultValue: "0")
            column(name: "creation_date", type: "DATETIME", defaultValueComputed: "CURRENT_TIMESTAMP")
            column(name: "last_updated_by", type: "BIGINT UNSIGNED", defaultValue: "0")
            column(name: "last_update_date", type: "DATETIME", defaultValueComputed: "CURRENT_TIMESTAMP")
        }
        createIndex(tableName: "agile_personal_filter", indexName: "agile_personal_filter_n1") {
            column(name: "project_id", type: "BIGINT UNSIGNED")
        }
    }

    changeSet(id: '2021-05-07-agile-personal-filter-add-column', author: 'ztxemail@163.com') {
        addColumn(tableName: 'agile_personal_filter') {
            column(name: 'is_default', type: 'TINYINT UNSIGNED', defaultValue: "0", remarks: '是否默认')
        }
    }

    changeSet(id: '2021-11-16-agile-personal-filter-add-column', author: 'huaxin.deng@hand-china.com') {
        addColumn(tableName: 'agile_personal_filter') {
            column(name: 'organization_id', type: 'BIGINT UNSIGNED', remarks: '组织id', defaultValue: '0') {
                constraints(nullable: false)
            }
            column(name: 'filter_type_code', type: 'VARCHAR(50)', defaultValue: "agile_issue", remarks: '个人筛选类型code') {
                constraints(nullable: false)
            }
        }
    }
}