package io.choerodon.agile.api.vo.report;

import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;

/**
 * @author jiaxu.cui@hand-china.com 2020/9/15 下午6:59
 */
@JsonTypeInfo(use = JsonTypeInfo.Id.NAME)
@JsonSubTypes({
        @JsonSubTypes.Type(name = "text",value = TextUnitVO.class),
        @JsonSubTypes.Type(name = "chart",value = ChartUnitVO.class),
        @JsonSubTypes.Type(name = "list",value = ListUnitVO.class)
})
public class ReportUnitVO {

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
