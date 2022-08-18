package script.db.groovy.agile_service

databaseChangeLog(logicalFilePath: 'fd_status_linkage.groovy') {
    changeSet(author: 'ztxemail@163.com', id: '2020-08-17-create-table-status-linkage') {
        createTable(tableName: 'fd_status_linkage', remarks: '状态联动父级联动表') {
            column(name: 'id', type: 'BIGINT UNSIGNED', autoIncrement: 'true', remarks: 'ID,主键') {
                constraints(primaryKey: true)
            }
            column(name: 'issue_type_id', type: 'BIGINT UNSIGNED', remarks: '问题类型Id') {
                constraints(nullable: false)
            }
            column(name: 'project_id', type: 'BIGINT UNSIGNED', remarks: '项目id') {
                constraints(nullable: false)
            }
            column(name: 'status_id', type: 'BIGINT UNSIGNED', remarks: '状态Id') {
                constraints(nullable: false)
            }
            column(name: 'parent_issue_type_code', type: 'VARCHAR(255)', remarks: '父级问题的类型code'){
                constraints(nullable: false)
            }
            column(name: 'parent_issue_status_setting', type: 'BIGINT UNSIGNED', remarks: '父级问题的状态Id'){
                constraints(nullable: false)
            }

            column(name: "object_version_number", type: "BIGINT UNSIGNED", defaultValue: "1")
            column(name: "created_by", type: "BIGINT UNSIGNED", defaultValue: "0")
            column(name: "creation_date", type: "DATETIME", defaultValueComputed: "CURRENT_TIMESTAMP")
            column(name: "last_updated_by", type: "BIGINT UNSIGNED", defaultValue: "0")
            column(name: "last_update_date", type: "DATETIME", defaultValueComputed: "CURRENT_TIMESTAMP")
        }
    }

    changeSet(author: 'ztxemail@163.com',id: '2020-09-14-fd-status-linkage-add-index'){
        createIndex(indexName: "idx_issue_type_id", tableName: "fd_status_linkage") {
            column(name: "issue_type_id")
        }
        createIndex(indexName: "idx_project_id", tableName: "fd_status_linkage") {
            column(name: "project_id")
        }
        createIndex(indexName: "idx_status_id", tableName: "fd_status_linkage") {
            column(name: "status_id")
        }
        createIndex(indexName: "idx_parent_issue_status_setting", tableName: "fd_status_linkage") {
            column(name: "parent_issue_status_setting")
        }
    }

    changeSet(author: 'kaiwen.li@hand-china.com',id: '2021-01-25-fd-status-linkage-add-column'){
        addColumn(tableName: 'fd_status_linkage') {
            column(name: 'parent_issue_type_id', type: 'BIGINT UNSIGNED', remarks: '父级问题的类型id', defaultValue: '0') {
                constraints(nullable: false)
            }
        }
    }

    changeSet(author: 'ztxemail@163.com',id: '2021-03-23-fd-status-linkage-add-column'){
        addColumn(tableName: 'fd_status_linkage') {
            column(name: 'organization_id', type: 'BIGINT UNSIGNED', remarks: '组织Id', defaultValue: "0")
        }
    }


    changeSet(author: 'ztxemail@163.com',id: '2021-12-27-fd-status-linkage-add-column'){
        addColumn(tableName: 'fd_status_linkage') {
            column(name: 'type', type: 'VARCHAR(255)', defaultValue: "all_transfer" , remarks: '联动校验类型：任一子任务流转（anyone_transfer）,全部子任务流转（all_transfer）')
        }
    }

    changeSet(author: 'tianxin.zhao@zknow.com',id: '2022-08-17-fd-status-linkage-fix-data'){
        sql(stripComments: true, splitStatements: true, endDelimiter: ';'){
            "delete fsl from fd_status_linkage fsl\n" +
                    "where \n" +
                    "fsl.issue_type_id not in (select fit.id from fd_issue_type fit)\n" +
                    "or fsl.parent_issue_type_id not in (select fit.id from fd_issue_type fit)"
        }

        sql(stripComments: true, splitStatements: true, endDelimiter: ';'){
            "DELETE fsns FROM fd_status_notice_setting fsns WHERE fsns.issue_type_id NOT IN (SELECT fit.id FROM fd_issue_type fit)"
        }

        sql(stripComments: true, splitStatements: true, endDelimiter: ';'){
            "DELETE fsfs FROM fd_status_field_setting fsfs WHERE fsfs.issue_type_id NOT IN (SELECT fit.id FROM fd_issue_type fit)"
        }

        sql(stripComments: true, splitStatements: true, endDelimiter: ';'){
            "DELETE fsrs FROM fd_status_transfer_setting fsrs WHERE fsrs.issue_type_id NOT IN (SELECT fit.id FROM fd_issue_type fit)"
        }
    }
}