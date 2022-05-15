package io.choerodon.agile.api.vo;

import io.swagger.annotations.ApiModelProperty;

import java.util.List;

/**
 * @author superlee
 * @since 2020-11-25
 */
public class GanttChartTreeVO {
    @ApiModelProperty(value = "概要")
    private String summary;
    @ApiModelProperty(value = "是否为组")
    private Boolean isGroup;
    @ApiModelProperty(value = "子级")
    private List<GanttChartVO> children;

    public Boolean getGroup() {
        return isGroup;
    }

    public void setGroup(Boolean group) {
        isGroup = group;
    }

    public String getSummary() {
        return summary;
    }

    public void setSummary(String summary) {
        this.summary = summary;
    }

    public List<GanttChartVO> getChildren() {
        return children;
    }

    public void setChildren(List<GanttChartVO> children) {
        this.children = children;
    }
}
