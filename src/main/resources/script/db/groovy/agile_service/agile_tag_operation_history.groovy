package script.db.groovy.agile_service
databaseChangeLog(logicalFilePath:'agile_tag_operation_history.groovy') {
    changeSet(id: '2021-05-19-agile-tag-operation-history', author: 'chihao.ran@hand-china.com') {
        createTable(tableName: "agile_tag_operation_history") {
            column(name: 'id', type: 'BIGINT UNSIGNED', autoIncrement: true, remarks: '主键id') {
                constraints(primaryKey: true)
            }
            column(name: 'project_id', type: 'BIGINT UNSIGNED', remarks: '项目id') {
                constraints(nullable: false)
            }
            column(name:"organization_id",type:"BIGINT UNSIGNED", remarks:"组织id", defaultValue: "0"){
                constraints(nullable: false)
            }
            column(name: 'publish_version_id', type: 'BIGINT UNSIGNED', remarks: 'publish_version_id'){
                constraints(nullable: false)
            }
            column(name: 'user_id', type: 'BIGINT UNSIGNED', remarks: '用户id')
            column(name: 'action', type: 'VARCHAR(255)', remarks: 'action')
            column(name: 'status', type: 'VARCHAR(255)', remarks: 'status')
            column(name: 'success_count', type: 'BIGINT UNSIGNED', remarks: 'success count')
            column(name: 'fail_count', type: 'BIGINT UNSIGNED', remarks: 'fail count')
            column(name: 'msg', type: 'VARCHAR(255)', remarks: 'msg')
            column(name: "object_version_number", type: "BIGINT UNSIGNED", defaultValue: "1")
            column(name: "created_by", type: "BIGINT UNSIGNED", defaultValue: "0")
            column(name: "creation_date", type: "DATETIME", defaultValueComputed: "CURRENT_TIMESTAMP")
            column(name: "last_updated_by", type: "BIGINT UNSIGNED", defaultValue: "0")
            column(name: "last_update_date", type: "DATETIME", defaultValueComputed: "CURRENT_TIMESTAMP")
        }
    }
}