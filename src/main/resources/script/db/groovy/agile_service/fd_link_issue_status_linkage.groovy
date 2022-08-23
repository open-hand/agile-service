package script.db.groovy.agile_service

databaseChangeLog(logicalFilePath: 'fd_link_issue_status_linkage.groovy') {
    changeSet(author: 'ztxemail@163.com', id: '2021-06-09-create-table-link-issue-status-linkage') {
        createTable(tableName: 'fd_link_issue_status_linkage', remarks: '关联issue状态联动表') {
            column(name: 'id', type: 'BIGINT UNSIGNED', autoIncrement: 'true', remarks: 'ID,主键') {
                constraints(primaryKey: true)
            }
            column(name: 'issue_type_id', type: 'BIGINT UNSIGNED', remarks: '问题类型id') {
                constraints(nullable: false)
            }
            column(name: 'status_id', type: 'BIGINT UNSIGNED', remarks: '状态id') {
                constraints(nullable: false)
            }
            column(name: 'link_type_id', type: 'BIGINT UNSIGNED', remarks: '关联类型id') {
                constraints(nullable: false)
            }
            column(name: 'link_issue_type_id', type: 'BIGINT UNSIGNED', remarks: '关联问题的问题类型id') {
                constraints(nullable: false)
            }
            column(name: 'link_issue_status_id', type: 'BIGINT UNSIGNED', remarks: '关联问题的状态') {
                constraints(nullable: false)
            }
            column(name: 'project_id', type: 'BIGINT UNSIGNED', remarks: '项目id') {
                constraints(nullable: false)
            }
            column(name: 'organization_id', type: 'BIGINT UNSIGNED', remarks: '组织id') {
                constraints(nullable: false)
            }

            column(name: "object_version_number", type: "BIGINT UNSIGNED", defaultValue: "1")
            column(name: "created_by", type: "BIGINT UNSIGNED", defaultValue: "0")
            column(name: "creation_date", type: "DATETIME", defaultValueComputed: "CURRENT_TIMESTAMP")
            column(name: "last_updated_by", type: "BIGINT UNSIGNED", defaultValue: "0")
            column(name: "last_update_date", type: "DATETIME", defaultValueComputed: "CURRENT_TIMESTAMP")
        }
    }

    changeSet(id: '2021-08-02-fd-link-issue-status-linkage-add-column', author: 'ztxemail@163.com') {
        addColumn(tableName: 'fd_link_issue_status_linkage') {
            column(name: 'is_triggered', type: 'TINYINT UNSIGNED', defaultValue: "0", remarks: '联动时是否触发关联的issue')
        }
    }

    changeSet(author: 'tianxin.zhao@zknow.com',id: '2022-08-22-fd_link_issue_status_linkage-fix-data'){
        sql(stripComments: true, splitStatements: true, endDelimiter: ';') {
            "DELETE fisl FROM fd_link_issue_status_linkage fisl WHERE fisl.issue_type_id NOT IN (SELECT fit.id FROM fd_issue_type fit)"
        }
    }
}