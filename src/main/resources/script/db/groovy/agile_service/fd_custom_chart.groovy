package script.db.groovy.agile_service

databaseChangeLog(logicalFilePath: 'script/db/fd_custom_chart.groovy') {
    def weight_c = 1
    if(helper.isOracle()){
        weight_c = 2
    }
    if(helper.isOracle()){
        weight_c = 3
    }
    changeSet(author: "chihao.ran@hand-china.com", id: "2021-06-23-fd-custom-chart"){
        if(helper.dbType().isSupportSequence()){
            createSequence(sequenceName: 'fd_custom_chart_s', startValue:"1")
        }
        createTable(tableName: "fd_custom_chart", remarks: "自定义报表") {
            column(name: "id", type: "bigint(20) unsigned",  remarks: "主键")  {constraints(primaryKey: true)}
            column(name: "name", type: "varchar(" + 255* weight_c + ")",  remarks: "自定义报表名称")  {constraints(nullable:"false")}  
            column(name: "description", type: "varchar(" + 255* weight_c + ")",  remarks: "自定义报表描述")   
            column(name: "analysis_field", type: "varchar(" + 255* weight_c + ")",  remarks: "分析维度参数")  {constraints(nullable:"false")}  
            column(name: "analysis_field_predefined", type: "tinyint",  remarks: "分析维度参数是否为预定义")  {constraints(nullable:"false")}  
            column(name: "compared_field", type: "varchar(" + 255* weight_c + ")",  remarks: "对比维度参数")   
            column(name: "compared_field_predefined", type: "tinyint",  remarks: "对比维度参数是否为预定义")   
            column(name: "statistics_type", type: "varchar(" + 255* weight_c + ")",  remarks: "统计项类型")  {constraints(nullable:"false")}  
            column(name: "chart_type", type: "varchar(" + 255* weight_c + ")",  remarks: "自定义报表图表类型")  {constraints(nullable:"false")}  
            column(name: "search_json", type: "text",  remarks: "自定义报表数据筛选参数（text）")
            column(name: "project_id", type: "bigint(20) unsigned",  remarks: "项目id")  {constraints(nullable:"false")}
            column(name: "organization_id", type: "bigint(20) unsigned",  remarks: "组织id")  {constraints(nullable:"false")}
            column(name: "created_by", type: "bigint(20) unsigned",  remarks: "创建人")  {constraints(nullable:"false")}
            column(name: "last_updated_by", type: "bigint(20) unsigned",  remarks: "最近更新人")  {constraints(nullable:"false")}
            column(name: "creation_date", type: "datetime",   defaultValueComputed :"CURRENT_TIMESTAMP",   remarks: "创建时间")  {constraints(nullable:"false")}  
            column(name: "last_update_date", type: "datetime",   defaultValueComputed :"CURRENT_TIMESTAMP",   remarks: "最近更新时间")  {constraints(nullable:"false")}
            column(name: "object_version_number", type: "bigint(20) unsigned",  remarks: "行版本号，用来处理锁")  {constraints(nullable:"false")}
        }
    }
}
