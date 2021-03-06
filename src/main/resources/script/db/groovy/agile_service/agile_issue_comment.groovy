package script.db.groovy.agile_service

databaseChangeLog(logicalFilePath: 'script/db/agile_issue_comment.groovy') {
    changeSet(id: '2018-05-14-agile-issue-comment', author: 'dinghuang123@gmail.com') {
        createTable(tableName: "agile_issue_comment", remarks: '敏捷开发Issue评论') {
            column(name: 'scrum_comment_id', type: 'BIGINT UNSIGNED', autoIncrement: true, remarks: '主键') {
                constraints(primaryKey: true)
            }
            column(name: 'user_id', type: 'BIGINT UNSIGNED', remarks: '用户id')
            column(name: 'issue_id', type: 'BIGINT UNSIGNED', remarks: 'issue的id') {
                constraints(nullable: false)
            }
            column(name: 'comment_text', type: 'text', remarks: '评论内容')
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
    changeSet(id: '2018-05-28-agile-issue-comment', author: 'dinghuang123@gmail.com') {
        renameColumn(columnDataType: 'BIGINT UNSIGNED', newColumnName: 'comment_id', oldColumnName: 'scrum_comment_id', remarks: '主键', tableName: 'agile_issue_comment')
        addAutoIncrement(schemaName: '', tableName: 'agile_issue_comment', columnName: 'comment_id', columnDataType: 'BIGINT UNSIGNED')
    }

    changeSet(id: '2018-06-06-agile-issue-comment-add-index', author: 'dinghuang123@gmail.com') {
        createIndex(indexName: "idx_issue_id", tableName: "agile_issue_comment") {
            column(name: "issue_id")
        }
    }

    changeSet( author: 'chihao.ran@hand-china.com',id: '2021-01-06-agile-issue-comment-add-column') {
        addColumn(tableName: "agile_issue_comment"){
            column(name:"parent_id",type:"BIGINT UNSIGNED",remarks:"父评论id", defaultValue: "0")
        }
    }

    changeSet( author: 'chihao.ran@hand-china.com',id: '2021-01-14-agile-issue-comment-add-column') {
        addColumn(tableName: "agile_issue_comment"){
            column(name:"reply_to_user_id",type:"BIGINT UNSIGNED",remarks:"被回复人id", defaultValue: "0")
        }
    }
}