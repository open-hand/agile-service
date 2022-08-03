package io.choerodon.agile.api.vo.report;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import io.choerodon.agile.infra.enums.ReportUnitType;
import org.hzero.core.base.BaseConstants;
import org.springframework.util.Assert;

/**
 * @author jiaxu.cui@hand-china.com 2020/9/15 下午6:59
 */
@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, property = "type", visible = true)
@JsonSubTypes({
        @JsonSubTypes.Type(name = ReportUnitType.TEXT,value = TextUnitVO.class),
        @JsonSubTypes.Type(name = ReportUnitType.CHART,value = ChartUnitVO.class),
        @JsonSubTypes.Type(name = ReportUnitType.STATIC_LIST,value = StaticListUnitVO.class),
        @JsonSubTypes.Type(name = ReportUnitType.STATIC_LIST_RISK,value = StaticListUnitVO.class),
        @JsonSubTypes.Type(name = ReportUnitType.DYNAMIC_LIST,value = DynamicListUnitVO.class),
        @JsonSubTypes.Type(name = ReportUnitType.DYNAMIC_LIST_RISK,value = DynamicListUnitVO.class)
})
@JsonInclude(JsonInclude.Include.NON_NULL)
public class ReportUnitVO {

    public void validateAndconvert(){
        Assert.notNull(title, BaseConstants.ErrorCode.DATA_INVALID);
        Assert.notNull(type, BaseConstants.ErrorCode.DATA_INVALID);
    }

    private String title;

    private String type;

    public String getTitle() {
        return title;
    }

    public void setTitle(String title) {
        this.title = title;
    }

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }
}
