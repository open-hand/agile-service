package script.db.groovy.agile_service

databaseChangeLog(logicalFilePath: 'script/db/agile_version_issue_rel.groovy') {
    changeSet(id: '2018-05-14-agile-version-issue-rel', author: 'jian_zhang02@163.com') {
        createTable(tableName: "agile_version_issue_rel", remarks: 'issue产品版本关系表') {
            column(name: 'version_id', type: 'BIGINT UNSIGNED', remarks: '版本id') {
                constraints(nullable: false)
            }
            column(name: 'issue_id', type: 'BIGINT UNSIGNED', remarks: '问题id') {
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
    changeSet(id: '2018-05-22-create-index', author: 'jian_zhang02@163.com') {
        createIndex(schemaName: '', tablespace: '', tableName: 'agile_version_issue_rel', indexName: 'uk_version_id_issue_id', unique: true) {
            column(name: 'version_id')
            column(name: 'issue_id')
        }
    }

    changeSet(id: '2018-05-24-agile-version-issue-rel', author: 'dinghuang123@gmail.com') {
        addColumn(tableName: 'agile_version_issue_rel') {
            column(name: 'relation_type', type: 'VARCHAR(32)', remarks: '版本类型')
        }
    }

    changeSet(id: '2018-06-05-agile-version-issue-rel', author: 'dinghuang123@gmail.com') {
        dropIndex(tableName: 'agile_version_issue_rel', indexName: 'uk_version_id_issue_id')
        createIndex(schemaName: '', tablespace: '', tableName: 'agile_version_issue_rel', indexName: 'uk_version_id_issue_id_relation_type', unique: true) {
            column(name: 'version_id')
            column(name: 'issue_id')
            column(name: 'relation_type')
        }
    }
    changeSet(id: '2018-06-07-agile-version-issue-rel-add-not-null-constraint', author: 'dinghuang123@gmail.com') {
        addNotNullConstraint(tableName: 'agile_version_issue_rel', columnName: 'relation_type', columnDataType: "VARCHAR(32)")
    }
    changeSet(id: '2018-06-08-agile-version-issue-rel-drop-index', author: 'dinghuang123@gmail.com') {
        dropIndex(tableName: 'agile_version_issue_rel', indexName: 'uk_version_id_issue_id_relation_type')
    }
    changeSet(id: '2018-06-08-agile-version-issue-rel-add-index', author: 'dinghuang123@gmail.com') {
        createIndex(schemaName: '', tablespace: '', tableName: 'agile_version_issue_rel', indexName: 'uk_version_id_issue_id_relation_type', unique: true) {
            column(name: 'version_id')
            column(name: 'issue_id')
            column(name: 'relation_type')
        }
    }

    changeSet(id: '2018-08-17-add-fix-version-data', author: 'dinghuang123@gmail.com') {
        sql(stripComments: true, splitStatements: false, endDelimiter: ';') {
            "UPDATE agile_version_issue_rel avir SET avir.object_version_number = 1,avir.created_by = 0,\n" +
                    "avir.creation_date = ( SELECT ai.creation_date from agile_issue ai where ai.issue_id = avir.issue_id ),\n" +
                    "avir.last_updated_by = 0,\n" +
                    "avir.last_update_date = (SELECT ai.creation_date from agile_issue ai where ai.issue_id = avir.issue_id)\n" +
                    "where avir.creation_date IS NULL;"
        }
    }

    changeSet(id: '2018-12-05-version-issue-rel-add-index', author: 'fuqianghuang01@gmail.com') {
        createIndex(tableName: "agile_version_issue_rel", indexName: "idx_issue_id") {
            column(name: "issue_id")
        }
    }

    changeSet(id: '2020-05-13-agile-version-issue-rel-add-column', author: 'fuqianghuang01@gmail.com') {
        addColumn(tableName: 'agile_version_issue_rel') {
            column(name: 'id', type: 'BIGINT UNSIGNED', autoIncrement: true, remarks: 'id') {
                constraints(primaryKey: true)
            }
        }
    }
}