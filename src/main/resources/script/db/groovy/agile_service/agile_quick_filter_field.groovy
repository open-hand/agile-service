package script.db.groovy.agile_service
databaseChangeLog(logicalFilePath:'agile_quick_filter_field.groovy'){
    changeSet(id: '2018-06-14-agile-quick-filter-field', author: 'fuqianghuang01@gmail.com') {
        createTable(tableName: "agile_quick_filter_field", remarks: '快速筛选字段表') {
            column(name: 'field_code', type: 'VARCHAR(255)',  remarks: '字段编码') {
                constraints(primaryKey: true)
            }
            column(name: 'type', type: 'VARCHAR(255)', remarks: '类型')
            column(name: 'name', type: 'VARCHAR(255)', remarks: '名称')
            column(name: 'field', type: 'VARCHAR(255)', remarks: '字段')

            column(name: "object_version_number", type: "BIGINT UNSIGNED", defaultValue: "1")
            column(name: "created_by", type: "BIGINT UNSIGNED", defaultValue: "0")
            column(name: "creation_date", type: "DATETIME", defaultValueComputed: "CURRENT_TIMESTAMP")
            column(name: "last_updated_by", type: "BIGINT UNSIGNED", defaultValue: "0")
            column(name: "last_update_date", type: "DATETIME", defaultValueComputed: "CURRENT_TIMESTAMP")
        }
    }
}