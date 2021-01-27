package script.db.groovy.agile_service

databaseChangeLog(logicalFilePath: 'fd_issue_type_extend.groovy') {
    changeSet(id: '2021-01-20-fd-issue-type-extend', author: 'kaiwen.li@hand-china.com') {
        createTable(tableName: 'fd_issue_type_extend', remarks: '问题类型扩展表，记录问题类型启停用') {
            column(name: 'id', type: 'BIGINT UNSIGNED', autoIncrement: 'true', remarks: 'ID,主键') {
                constraints(primaryKey: 'true')
            }
            column(name: 'issue_type_id', type: 'BIGINT UNSIGNED', remarks: '问题类型id') {
                constraints(nullable: false)
            }
            column(name: 'project_id', type: 'BIGINT UNSIGNED', remarks: '项目id') {
                constraints(nullable: false)
            }
            column(name: 'organization_id', type: 'BIGINT UNSIGNED', remarks: '组织id') {
                constraints(nullable: false)
            }
            column(name: 'enabled', type: 'TINYINT UNSIGNED', remarks: '启停用') {
                constraints(nullable: false)
            }

            column(name: "object_version_number", type: "BIGINT UNSIGNED", defaultValue: "1")
            column(name: "created_by", type: "BIGINT UNSIGNED", defaultValue: "0")
            column(name: "creation_date", type: "DATETIME", defaultValueComputed: "CURRENT_TIMESTAMP")
            column(name: "last_updated_by", type: "BIGINT UNSIGNED", defaultValue: "0")
            column(name: "last_update_date", type: "DATETIME", defaultValueComputed: "CURRENT_TIMESTAMP")
        }
    }

    changeSet(id: '2021-01-27-fd-issue-type-extend-add-column', author: 'kaiwen.li@hand-china.com') {
        addColumn(tableName: 'fd_issue_type_extend') {
            column(name: 'name', type: 'VARCHAR(64)', remarks: '名称，用于系统问题类型重命名', afterColumn: 'enabled')
            column(name: 'description', type: 'VARCHAR(255)', remarks: '描述，用于系统问题类型更改描述', afterColumn: 'name')
            column(name: 'icon', type: 'VARCHAR(64)', remarks: '图标，用于系统问题类型更改图标', afterColumn: 'description')
            column(name: 'colour', type: 'VARCHAR(20)', remarks: '颜色，用于系统问题类型更改颜色', afterColumn: 'icon')
        }
    }
}
