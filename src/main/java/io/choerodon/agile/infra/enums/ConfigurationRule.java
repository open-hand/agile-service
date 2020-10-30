package io.choerodon.agile.infra.enums;

import java.util.ArrayList;
import java.util.List;

import io.choerodon.agile.api.vo.FieldTableVO;
import org.apache.commons.lang3.StringUtils;

/**
 * 页面规则 常量类
 * @author jiaxu.cui@hand-china.com 2020/9/23 下午2:36
 */
public class ConfigurationRule{
    
    public static final String TEMPLATE_LINK_TABLE_SQL = " issue_id %s ( SELECT issue_id FROM %s %s ) ";
    public static final String TEMPLATE_IN_SQL = " %s IN ( %s ) ";
    public static final String TEMPLATE_LIKE_VALUE_SQL = " '%%%s%%' ";
    public static final String TEMPLATE_SQL_WHERE = " WHERE ";
    public static final String TEMPLATE_SQL_AND = " AND ";
    public static final String TEMPLATE_CONDITION_SQL = " %s %s %s ";
    public static final String TEMPLATE_UNIX_TIMESTAMP_EXPRESS = " UNIX_TIMESTAMP(%s) ";
    public static final String TEMPLATE_TIME_FIELD_EXPRESS = " TIME(DATE_FORMAT(%s, '%%H:%%i:%%s')) ";
    public static final String TEMPLATE_TIME_VALUE_EXPRESS = " TIME(%s) ";
    public static final String SQL_VAR_NOW_EXPRESS = "NOW()";
    public static final String FIELD_ISSUE_ID = "issue_id";
    public static final String FIELD_BACKLOG_ID = "id";
    public static final String TEMPLATE_CUSTOM_SQL = " %s %s ( select ffv.instance_id from fd_field_value ffv where ffv.project_id = %s and ffv.field_id = %s %s ) ";
    public static final String SQL_VAR_NOT_EQUALS = " 1 = 2 ";
    public static final String TEMPLATE_TYPE_LIMIT = " (" + TEMPLATE_CONDITION_SQL + ") ";
    
    public static final List<FieldTableVO> fieldTableList = new ArrayList<>(18);
    
    public static boolean isSqlVar(String express){
        return StringUtils.equalsAnyIgnoreCase(express, SQL_VAR_NOW_EXPRESS);
    }
    
    static {
        fieldTableList.add(new FieldTableVO("component", "component_id", "agile_component_issue_rel"));
        fieldTableList.add(new FieldTableVO("fixVersion","version_id","agile_version_issue_rel"));
        fieldTableList.add(new FieldTableVO("influenceVersion","version_id","agile_version_issue_rel"));
        fieldTableList.add(new FieldTableVO("label","label_id","agile_label_issue_rel"));
        fieldTableList.add(new FieldTableVO("sprint","sprint_id","agile_issue_sprint_rel"));
        fieldTableList.add(new FieldTableVO("assignee","assignee_id","agile_issue"));
        fieldTableList.add(new FieldTableVO("priority","priority_id","agile_issue"));
        fieldTableList.add(new FieldTableVO("status","status_id","agile_issue"));
        fieldTableList.add(new FieldTableVO("reporter","reporter_id","agile_issue"));
        fieldTableList.add(new FieldTableVO("creationDate","creation_date","agile_issue"));
        fieldTableList.add(new FieldTableVO("lastUpdateDate","last_update_date","agile_issue"));
        fieldTableList.add(new FieldTableVO("storyPoints","story_point","agile_issue"));
        fieldTableList.add(new FieldTableVO("remainingTime","remaining_time","agile_issue"));
        fieldTableList.add(new FieldTableVO("epic","epic_id","agile_issue"));
        fieldTableList.add(new FieldTableVO("issueType","type_code","agile_issue"));
        fieldTableList.add(new FieldTableVO("estimatedStartTime","estimated_start_time","agile_issue"));
        fieldTableList.add(new FieldTableVO("estimatedEndTime","estimated_end_time","agile_issue"));
        fieldTableList.add(new FieldTableVO("issue","issue","agile_issue"));
    }

    public enum OpSqlMapping {
        in("IN"),
        not_in("NOT IN"),
        is("IS"){
            @Override
            public OpSqlMapping withField(String field) {
                if (StringUtils.equalsAny(field, "version_id", "component_id", "label_id", "sprint_id")){
                    return not_in;
                }
                return this;
            }
        },
        is_not("IS NOT"){
            @Override
            public OpSqlMapping withField(String field) {
                if (StringUtils.equalsAny(field, "version_id", "component_id", "label_id", "sprint_id")){
                    return in;
                }
                return this;
            }
        },
        eq("="),
        not_eq("!="),
        gt(">"),
        lt("<"),
        gte(">="),
        lte("<="),
        like("LIKE"),
        not_like("NOT LIKE"),
        and("AND"),
        or("OR");
        
        OpSqlMapping(String sqlOp) {
            this.sqlOp = sqlOp;
        }
        
        private String sqlOp;
        
        public String getSqlOp() {
            return sqlOp;
        }

        public OpSqlMapping withField(String field){
            return this;
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

        public static boolean isNullKey(String op){
            if (OpSqlMapping.is.name().equals(op) || OpSqlMapping.is_not.name().equals(op)){
                return true;
            }else {
                return false;
            }
        }
    }
}

