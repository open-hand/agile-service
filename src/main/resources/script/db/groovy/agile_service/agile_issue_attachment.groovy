package script.db.groovy.agile_service

databaseChangeLog(logicalFilePath: 'agile_issue_attachment.groovyoovy') {
    changeSet(id: '2018-05-14-agile-issue-attachment', author: 'fuqianghuang01@gmail.com') {
        createTable(tableName: "agile_issue_attachment", remarks: 'issue附件表') {
            column(name: 'attachment_id', type: 'BIGINT UNSIGNED', autoIncrement: true, remarks: '附件id') {
                constraints(primaryKey: true)
            }
            column(name: 'issue_id', type: 'BIGINT UNSIGNED', remarks: '问题id') {
                constraints(nullable: false)
            }
            column(name: 'comment_id', type: 'BIGINT UNSIGNED', remarks: '评论id') {
                constraints(nullable: false)
            }
            column(name: 'url', type: 'VARCHAR(255)', remarks: 'url') {
                constraints(nullable: false)
            }
            column(name: 'file_name', type: 'VARCHAR(255)', remarks: '文件名称') {
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

    changeSet(id: '2018-06-06-agile-issue-attachment-add-index', author: 'dinghuang123@gmail.com') {
        createIndex(indexName: "idx_issue_id", tableName: "agile_issue_attachment") {
            column(name: "issue_id")
        }
    }
}