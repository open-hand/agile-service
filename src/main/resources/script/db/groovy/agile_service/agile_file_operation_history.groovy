package script.db.groovy.agile_service
databaseChangeLog(logicalFilePath:'agile_file_operation_history.groovy') {
    changeSet(id: '2019-02-25-agile-file-operation-history', author: 'fuqianghuang01@gmail.com') {
        createTable(tableName: "agile_file_operation_history") {
            column(name: 'id', type: 'BIGINT UNSIGNED', autoIncrement: true, remarks: 'id') {
                constraints(primaryKey: true)
            }
            column(name: 'project_id', type: 'BIGINT UNSIGNED', remarks: '项目id') {
                constraints(nullable: false)
            }
            column(name: 'action', type: 'VARCHAR(255)', remarks: '行为')
            column(name: 'success_count', type: 'BIGINT UNSIGNED', remarks: '成功数量')
            column(name: 'fail_count', type: 'BIGINT UNSIGNED', remarks: '失败数量')
            column(name: 'status', type: 'VARCHAR(255)', remarks: '状态')
            column(name: 'file_url', type: 'VARCHAR(1000)', remarks: '文件url')
            column(name: 'user_id', type: 'BIGINT UNSIGNED', remarks: '操作人id')

            column(name: "object_version_number", type: "BIGINT UNSIGNED", defaultValue: "1")
            column(name: "created_by", type: "BIGINT UNSIGNED", defaultValue: "0")
            column(name: "creation_date", type: "DATETIME", defaultValueComputed: "CURRENT_TIMESTAMP")
            column(name: "last_updated_by", type: "BIGINT UNSIGNED", defaultValue: "0")
            column(name: "last_update_date", type: "DATETIME", defaultValueComputed: "CURRENT_TIMESTAMP")
        }
    }

    changeSet(id: '2021-03-01-agile-file-operation-history-add-column', author: 'chihao.ran@hand-china.com') {
        addColumn(tableName: "agile_file_operation_history"){
            column(name:"organization_id",type:"BIGINT UNSIGNED", remarks:"组织id", defaultValue: "0"){
                constraints(nullable: false)
            }
        }
    }
}