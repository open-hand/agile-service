package script.db.groovy.agile_service

databaseChangeLog(logicalFilePath: 'agile_static_file_header.groovy') {
    changeSet(id: '2021-01-13-agile-static-file-header', author: 'chihao.ran@hand-china.com') {
        createTable(tableName: "agile_static_file_header", remarks: '敏捷静态文件头列表') {
            column(name: 'id', type: 'BIGINT UNSIGNED', autoIncrement: true, remarks: 'id') {
                constraints(primaryKey: true)
            }
            column(name: 'project_id', type: 'BIGINT UNSIGNED', remarks: '项目id') {
                constraints(nullable: false)
            }
            column(name: 'organization_id', type: 'BIGINT UNSIGNED', remarks: '组织id') {
                constraints(nullable: false)
            }
            column(name: 'url', type: 'VARCHAR(500)', remarks: 'url') {
                constraints(nullable: false)
            }
            column(name: 'file_name', type: 'VARCHAR(500)', remarks: '文件名称') {
                constraints(nullable: false)
            }
            column(name: "status", type:"VARCHAR(255)", remarks:"静态文件状态(failed、success、doing)")

            column(name: "object_version_number", type: "BIGINT UNSIGNED", defaultValue: "1")
            column(name: "created_by", type: "BIGINT UNSIGNED", defaultValue: "0")
            column(name: "creation_date", type: "DATETIME", defaultValueComputed: "CURRENT_TIMESTAMP")
            column(name: "last_updated_by", type: "BIGINT UNSIGNED", defaultValue: "0")
            column(name: "last_update_date", type: "DATETIME", defaultValueComputed: "CURRENT_TIMESTAMP")
        }
    }
}