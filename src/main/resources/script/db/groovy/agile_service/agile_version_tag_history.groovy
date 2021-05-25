package script.db.groovy.agile_service
databaseChangeLog(logicalFilePath:'agile_version_tag_history.groovy') {
    changeSet(id: '2021-05-19-agile-version-tag-history', author: 'chihao.ran@hand-china.com') {
        createTable(tableName: "agile_version_tag_history") {
            column(name: 'id', type: 'BIGINT UNSIGNED', autoIncrement: true, remarks: '主键id') {
                constraints(primaryKey: true)
            }
            column(name: 'version_id', type: 'BIGINT UNSIGNED', remarks: '版本id'){
                constraints(nullable: false)
            }
            column(name: 'version_type', type: 'VARCHAR(32)', remarks: 'program为项目群版本Id，publish为发布版本id'){
                constraints(nullable: false)
            }
            column(name: 'user_id', type: 'BIGINT UNSIGNED', remarks: '用户id')
            column(name: 'action', type: 'VARCHAR(255)', remarks: 'action')
            column(name: 'status', type: 'VARCHAR(255)', remarks: 'status')
            column(name: 'project_id', type: 'BIGINT UNSIGNED', remarks: '项目id') {
                constraints(nullable: false)
            }
            column(name:"organization_id",type:"BIGINT UNSIGNED", remarks:"组织id", defaultValue: "0"){
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