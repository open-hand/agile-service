package script.db.groovy.agile_service
databaseChangeLog(logicalFilePath:'agile_work_calendar_subscribe.groovy') {
    changeSet(id: '2021-10-12-agile-work-calendar-subscribe', author: 'huaxin.deng@hand-china.com') {
        createTable(tableName: "agile_work_calendar_subscribe", remarks: '工作日历订阅表') {
            column(name: 'id', type: 'BIGINT UNSIGNED', autoIncrement: true, remarks: '主键') {
                constraints(primaryKey: true)
            }
            column(name: 'user_id', type: 'BIGINT UNSIGNED', remarks: '用户id')  {
                constraints(nullable: false)
            }
            column(name: 'organization_id', type: 'BIGINT UNSIGNED', remarks: '组织id') {
                constraints(nullable: false)
            }
            column(name: 'uuid', type: 'VARCHAR(255)', remarks: 'uuid') {
                constraints(nullable: false)
            }
            column(name: 'file_url', type: 'VARCHAR(255)', remarks: '文件url') {
                constraints(nullable: false)
            }
            column(name: 'is_changed', type: 'TINYINT UNSIGNED(3)', remarks: '是否发生改变', defaultValue: "0") {
                constraints(nullable: false)
            }

            column(name: "object_version_number", type: "BIGINT UNSIGNED", defaultValue: "1")
            column(name: "created_by", type: "BIGINT UNSIGNED", defaultValue: "0")
            column(name: "creation_date", type: "DATETIME", defaultValueComputed: "CURRENT_TIMESTAMP")
            column(name: "last_updated_by", type: "BIGINT UNSIGNED", defaultValue: "0")
            column(name: "last_update_date", type: "DATETIME", defaultValueComputed: "CURRENT_TIMESTAMP")
        }

        createIndex(indexName: 'idx_user_id', tableName: 'agile_work_calendar_subscribe') {
            column(name: 'user_id')
        }
        createIndex(indexName: "idx_organization_id", tableName: "agile_work_calendar_subscribe") {
            column(name: "organization_id")
        }
        createIndex(indexName: "uk_uuid", tableName: "agile_work_calendar_subscribe", unique: true) {
            column(name: "uuid")
        }
    }
}