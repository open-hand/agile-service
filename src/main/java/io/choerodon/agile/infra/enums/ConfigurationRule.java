package io.choerodon.agile.infra.enums;

/**
 * 页面规则 常量类
 * @author jiaxu.cui@hand-china.com 2020/9/23 下午2:36
 */
public class ConfigurationRule{
    
    public static final String TEMPLATE_PREDEFINED_SQL = " issue_id %s ( SELECT issue_id FROM %s %s ) ";
    public static final String TEMPLATE_IN_SQL = " %s IN ( %s ) ";
    public static final String TEMPLATE_LIKE_VALUE_SQL = " '%%%%%s%%%%' ";
    public static final String TEMPLATE_SQL_WHERE = " WHERE ";
    public static final String TEMPLATE_SQL_AND = " AND ";
    public static final String TEMPLATE_CONDITION_SQL = " %s %s %s ";
    public static final String TEMPLATE_UNIX_TIMESTAMP_EXPRESS = " UNIX_TIMESTAMP(%s) ";
    public static final String TEMPLATE_TIME_FIELD_EXPRESS = " TIME(DATE_FORMAT(%s, '%%H:%%i:%%s')) ";
    public static final String TEMPLATE_TIME_VALUE_EXPRESS = " TIME(%s) ";
    public static final String TEMPLATE_CUSTOM_SQL = " issue_id %s ( select ffv.instance_id from fd_field_value ffv where ffv.project_id = %s and ffv.field_id = %s %s ) ";
    
    
    public enum FieldTableMapping {

        component_id("agile_component_issue_rel"),
        version_id("agile_version_issue_rel"),
        label_id("agile_label_issue_rel"),
        sprint_id("agile_issue_sprint_rel")
        ;

        FieldTableMapping(String table) {
            this.table = table;
        }

        private String table;

        public String getTable() {
            return table;
        }
    }

    public enum OpSqlMapping {
        in("IN"),
        not_in("NOT IN"),
        is("NOT IN"),
        is_not("IN"),
        eq("="),
        not_eq("!="),
        gt("&gt;"),
        lt("&lt;"),
        gte("&gt;="),
        lte("&lt;="),
        like("LIKE"),
        not_like("NOT LIKE"),
        and("AND"),
        or("or")
        ;

        OpSqlMapping(String sqlOp) {
            this.sqlOp = sqlOp;
        }
        
        private String sqlOp;
        
        public String getSqlOp() {
            return sqlOp;
        }
        
        public static boolean isCollOp(String op){
            boolean flag = false;
            if (OpSqlMapping.in.name().equals(op)){
                flag = true;
            }
            if (OpSqlMapping.not_in.name().equals(op)){
                flag = true;
            }
            if (OpSqlMapping.is.name().equals(op)){
                flag = true;
            }
            if (OpSqlMapping.is_not.name().equals(op)){
                flag = true;
            }
            return flag;
        }

        public static OpSqlMapping getPreOp(String op){
            if (OpSqlMapping.not_in.name().equals(op) || OpSqlMapping.is.name().equals(op) || OpSqlMapping.not_like.name().equals(op)){
                return OpSqlMapping.not_in;
            }else {
                return OpSqlMapping.in;
            }
        }

        public static boolean isLike(String op){
            if (OpSqlMapping.like.name().equals(op) || OpSqlMapping.not_like.name().equals(op)){
                return true;
            }else {
                return false;
            }
        }
    }
}

