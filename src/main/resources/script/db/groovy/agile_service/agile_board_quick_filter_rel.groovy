package script.db.groovy.agile_service
databaseChangeLog(logicalFilePath:'agile_board_quick_filter_rel.groovy') {
    changeSet(id: '2021-11-23-agile-board-quick-filter-rel', author: 'kaiwen.li@hand-china.com') {
        createTable(tableName: "agile_board_quick_filter_rel") {
            column(name: 'id', type: 'BIGINT UNSIGNED', autoIncrement: true, remarks: 'id') {
                constraints(primaryKey: true)
            }
            column(name: 'board_id', type: 'BIGINT UNSIGNED', remarks: '看板id') {
                constraints(nullable: false)
            }
            column(name: 'quick_filter_id', type: 'BIGINT UNSIGNED', remarks: '快速筛选id') {
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
}