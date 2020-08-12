package script.db

databaseChangeLog(logicalFilePath: 'script/db/fd_ status_notice_setting.groovy') {
    changeSet(author: "jiaxu.cui@hand-china.com", id: "2020-08-12-fd_ status_notice_setting") {
        createTable(tableName: "fd_ status_notice_setting", remarks: "邮件通知") {
            column(name: "id", type: "bigint(20) unsigned", autoIncrement: true ,   remarks: "")  {constraints(nullable:"false")}
            column(name: "issue_type_id", type: "bigint(20) unsigned",  remarks: "")  {constraints(nullable:"false")}
            column(name: "project_id", type: "bigint(20) unsigned",  remarks: "")  {constraints(nullable:"false")}
            column(name: "status_id", type: "bigint(20) unsigned",  remarks: "")  {constraints(nullable:"false")}
            column(name: "user_type", type: "varchar(255)",  remarks: "")  {constraints(nullable:"false")}
            column(name: "user_id", type: "bigint(20) unsigned",  remarks: "")
            column(name: "notice_type", type: "varchar(255)",  remarks: "projectOwner, assignee, reporter, specifier")  {constraints(nullable:"false")}
            column(name: "object_version_number", type: "bigint(20) unsigned",   defaultValue:"1",   remarks: "")
            column(name: "created_by", type: "bigint(20) unsigned",   defaultValue:"0",   remarks: "")
            column(name: "creation_date", type: "datetime",   defaultValueComputed:"CURRENT_TIMESTAMP",   remarks: "")
            column(name: "last_updated_by", type: "bigint(20) unsigned",   defaultValue:"0",   remarks: "")
            column(name: "last_update_date", type: "datetime",   defaultValueComputed:"CURRENT_TIMESTAMP",   remarks: "")

        }

    }
}