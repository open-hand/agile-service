package script.db.groovy.agile_service
databaseChangeLog(logicalFilePath:'agile_issue_branch.groovyoovy') {
    changeSet(id: '2018-05-14-agile-issue-branch', author: 'fuqianghuang01@gmail.com') {
        createTable(tableName: "agile_issue_branch") {
            column(name: 'branch_id', type: 'BIGINT UNSIGNED', autoIncrement: true, remarks: '分支id') {
                constraints(primaryKey: true)
            }
            column(name: 'branch_type', type: 'VARCHAR(30)', remarks: '分支类型') {
                constraints(nullable: false)
            }
            column(name: 'issue_id', type: 'BIGINT UNSIGNED', remarks: '问题id') {
                constraints(nullable: false)
            }
            column(name: 'branch_name', type: 'VARCHAR(255)', remarks: '分支名称') {
                constraints(nullable: false)
            }
            column(name: 'application_id', type: 'BIGINT UNSIGNED', remarks: '应用id') {
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
}