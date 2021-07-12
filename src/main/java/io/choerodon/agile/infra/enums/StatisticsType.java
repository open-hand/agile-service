package io.choerodon.agile.infra.enums;

import org.apache.commons.lang.StringUtils;

/**
 * @author chihao.ran@hand-china.com
 * 2021/06/11 11:34
 */
public enum StatisticsType {
    //数量
    COUNT("COUNT(1) AS `value`"),
    STORY_POINT("SUM(IFNULL(ai.story_points, 0)) AS `value`"),
    ;
    String valueSql;


    StatisticsType(String valueSql) {
        this.valueSql = valueSql;
    }

    public String getValueSql() {
        return valueSql;
    }

    public static String getSqlByType(String type){
        if(StringUtils.isEmpty(type)){
            return null;
        }
        switch (type){
            case "quantity":
                return COUNT.valueSql;
            case "storyPoints":
                return STORY_POINT.valueSql;
            default:break;
        }
        return null;
    }
}