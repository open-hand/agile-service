package script.db.groovy.agile_service
databaseChangeLog(logicalFilePath:'agile_message_detail.groovy') {
    changeSet(id: '2018-08-14-agile-message-detail', author: 'fuqianghuang01@gmail.com') {
        createTable(tableName: "agile_message_detail", remarks: '敏捷消息详情表') {
            column(name: 'id', type: 'BIGINT UNSIGNED', autoIncrement: true, remarks: 'id') {
                constraints(primaryKey: true)
            }
            column(name: 'project_id', type: 'BIGINT UNSIGNED', remarks: '项目id') {
                constraints(nullable: false)
            }
            column(name: 'event', type: 'VARCHAR(255)', remarks: '事件') {
                constraints(nullable: false)
            }
            column(name: 'notice_type', type: 'VARCHAR(255)', remarks: '统计类型') {
                constraints(nullable: false)
            }
            column(name: 'notice_name', type: 'VARCHAR(255)', remarks: '通知名称') {
                constraints(nullable: false)
            }
            column(name: 'is_enable', type: 'TINYINT UNSIGNED', remarks: '是否启停用') {
                constraints(nullable: false)
            }
            column(name: 'user', type: 'VARCHAR(1000)', remarks: '用户')

            column(name: "object_version_number", type: "BIGINT UNSIGNED", defaultValue: "1")
            column(name: "created_by", type: "BIGINT UNSIGNED", defaultValue: "0")
            column(name: "creation_date", type: "DATETIME", defaultValueComputed: "CURRENT_TIMESTAMP")
            column(name: "last_updated_by", type: "BIGINT UNSIGNED", defaultValue: "0")
            column(name: "last_update_date", type: "DATETIME", defaultValueComputed: "CURRENT_TIMESTAMP")
        }
    }
}