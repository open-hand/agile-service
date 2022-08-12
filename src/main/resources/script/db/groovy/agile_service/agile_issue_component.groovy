package script.db.groovy.agile_service
databaseChangeLog(logicalFilePath:'agile_issue_component.groovyoovy') {
    changeSet(id: '2018-05-14-agile-issue-component', author: 'fuqianghuang01@gmail.com') {
        createTable(tableName: "agile_issue_component", remarks: '敏捷模块表') {
            column(name: 'component_id', type: 'BIGINT UNSIGNED', autoIncrement: true, remarks: 'id') {
                constraints(primaryKey: true)
            }
            column(name: 'project_id', type: 'BIGINT UNSIGNED', remarks: '项目id') {
                constraints(nullable: false)
            }
            column(name: 'name', type: 'VARCHAR(255)', remarks: '名称') {
                constraints(nullable: false)
            }
            column(name: 'description', type: 'VARCHAR(5000)', remarks: '描述')
            column(name: "object_version_number", type: "BIGINT UNSIGNED", defaultValue: "1")
            column(name: "created_by", type: "BIGINT UNSIGNED", defaultValue: "0")
            column(name: "creation_date", type: "DATETIME", defaultValueComputed: "CURRENT_TIMESTAMP")
            column(name: "last_updated_by", type: "BIGINT UNSIGNED", defaultValue: "0")
            column(name: "last_update_date", type: "DATETIME", defaultValueComputed: "CURRENT_TIMESTAMP")
        }
    }
    changeSet(id:  '2018-05-31-agile-issue-component-add-column', author: 'fuqianghuang01@gmail.com') {
        addColumn(tableName: 'agile_issue_component') {
            column(name: 'manager_id', type: 'BIGINT UNSIGNED', remarks: '管理人id')
            column(name: 'default_assignee_role', type: 'VARCHAR(255)', remarks: '默认经办人角色')
        }
    }

    changeSet(id:  '2021-01-27-agile-issue-component-add-column', author: 'ztxemail@163.com') {
        addColumn(tableName: 'agile_issue_component') {
            column(name: 'sequence', type: 'INTEGER UNSIGNED',  remarks: '模块排序')
        }
    }
}