package script.db.groovy.agile_service

databaseChangeLog(logicalFilePath: 'script/db/fd_custom_chart.groovy') {
    changeSet(author: "chihao.ran@hand-china.com", id: "2021-06-23-fd-custom-chart"){
        createTable(tableName: "fd_custom_chart", remarks: "自定义报表") {
            column(name: "id", type: "BIGINT(20) UNSIGNED", remarks: "主键") {
                constraints(primaryKey: true)
            }
            column(name: "name", type: "VARCHAR(255)", remarks: "自定义报表名称") {
                constraints(nullable:"false")
            }
            column(name: "description", type: "VARCHAR(255)", remarks: "自定义报表描述")   
            column(name: "analysis_field", type: "VARCHAR(255)", remarks: "分析维度参数") {
                constraints(nullable:"false")
            }
            column(name: "analysis_field_predefined", type: "TINYINT(1)", remarks: "分析维度参数是否为预定义") {
                constraints(nullable:"false")
            }
            column(name: "compared_field", type: "VARCHAR(255)", remarks: "对比维度参数")   
            column(name: "compared_field_predefined", type: "TINYINT(1)", remarks: "对比维度参数是否为预定义")
            column(name: "statistics_type", type: "VARCHAR(255)", remarks: "统计项类型") {
                constraints(nullable:"false")
            }
            column(name: "chart_type", type: "VARCHAR(255)", remarks: "自定义报表图表类型") {
                constraints(nullable:"false")
            }
            column(name: "search_json", type: "TEXT", remarks: "自定义报表数据筛选参数（TEXT）")
            column(name: "project_id", type: "BIGINT(20) UNSIGNED", remarks: "项目id") {
                constraints(nullable:"false")
            }
            column(name: "organization_id", type: "BIGINT(20) UNSIGNED", remarks: "组织id") {
                constraints(nullable:"false")
            }

            column(name: "created_by", type: "BIGINT(20) UNSIGNED", remarks: "创建人")
            column(name: "last_updated_by", type: "BIGINT(20) UNSIGNED", remarks: "最近更新人")
            column(name: "creation_date", type: "DATETIME", defaultValueComputed :"CURRENT_TIMESTAMP", remarks: "创建时间")
            column(name: "last_update_date", type: "DATETIME", defaultValueComputed :"CURRENT_TIMESTAMP", remarks: "最近更新时间")
            column(name: "object_version_number", type: "BIGINT(20) UNSIGNED", remarks: "行版本号，用来处理锁")
        }
    }
}
