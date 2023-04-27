package script.db.groovy.agile_service

/**
 * 用户偏好设置
 * @author gaokuo.dai@zknow.com 2023-03-30
 * @since 2.4
 */
databaseChangeLog(logicalFilePath: 'agile_user_preference.groovy') {
    changeSet(author: "gaokuo.dai@zknow.com", id: "2023-03-30-agile_user_preference") {
        def weight = 1
        if (helper.isSqlServer()) {
            weight = 2
        } else if (helper.isOracle()) {
            weight = 3
        }
        if (helper.dbType().isSupportSequence()) {
            createSequence(sequenceName: 'agile_user_preference_s', startValue: "1")
        }

        createTable(tableName: "agile_user_preference", remarks: "用户偏好设置") {
            column(name: "id", type: "bigint", autoIncrement: true, remarks: "主键") { constraints(primaryKey: true) }
            column(name: "organization_id", type: "bigint", defaultValue: "0", remarks: "组织ID") { constraints(nullable: "false") }
            column(name: "project_id", type: "bigint", defaultValue: "0", remarks: "项目ID") { constraints(nullable: "false") }
            column(name: "user_id", type: "bigint", remarks: "用户ID") { constraints(nullable: "false") }
            column(name: "preference_key", type: "varchar(" + 120 * weight + ")", remarks: "偏好设置键，用于表示设置项") { constraints(nullable: "false") }
            column(name: "preference_value", type: "varchar(" + 480 * weight + ")", remarks: "偏好设置值，用于表示设置对应的值")
            column(name: "object_version_number", type: "bigint", defaultValue: "1", remarks: "") { constraints(nullable: "false") }
            column(name: "created_by", type: "bigint", defaultValue: "0", remarks: "") { constraints(nullable: "false") }
            column(name: "creation_date", type: "datetime", defaultValueComputed: "CURRENT_TIMESTAMP", remarks: "") { constraints(nullable: "false") }
            column(name: "last_updated_by", type: "bigint", defaultValue: "0", remarks: "") { constraints(nullable: "false") }
            column(name: "last_update_date", type: "datetime", defaultValueComputed: "CURRENT_TIMESTAMP", remarks: "") { constraints(nullable: "false") }
        }

        addUniqueConstraint(columnNames: "organization_id,project_id,user_id,preference_key", tableName: "agile_user_preference", constraintName: "agile_user_preference_U1")
    }
}
