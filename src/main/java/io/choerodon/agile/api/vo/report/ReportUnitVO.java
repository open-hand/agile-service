package io.choerodon.agile.api.vo.report;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import org.hzero.core.base.BaseConstants;
import org.springframework.util.Assert;

/**
 * @author jiaxu.cui@hand-china.com 2020/9/15 下午6:59
 */
@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, property = "type", visible = true)
@JsonSubTypes({
        @JsonSubTypes.Type(name = "text",value = TextUnitVO.class),
        @JsonSubTypes.Type(name = "chart",value = ChartUnitVO.class),
        @JsonSubTypes.Type(name = "static_list",value = StaticListUnitVO.class),
        @JsonSubTypes.Type(name = "dynamic_list",value = StaticListUnitVO.class)
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
