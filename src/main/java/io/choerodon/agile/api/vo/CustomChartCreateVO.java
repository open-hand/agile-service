package io.choerodon.agile.api.vo;

import io.swagger.annotations.ApiModelProperty;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;

/**
 * @author chihao.ran@hand-china.com
 * 2021/06/21 16:40
 */
public class CustomChartCreateVO {
    @NotBlank(message = "error.customChart.nameNotNull")
    @ApiModelProperty(value = "自定义报表名称", required = true)
    private String name;
    @ApiModelProperty(value = "描述")
    private String description;
    @NotBlank(message = "error.customChart.analysisFieldNotNull")
    @ApiModelProperty(value = "分析维度字段", required = true)
    private String analysisField;
    @NotNull(message = "error.customChart.analysisFieldPredefinedNotNull")
    @ApiModelProperty(value = "分析维度字段是否为预定义字段", required = true)
    private Boolean analysisFieldPredefined;
    @ApiModelProperty(value = "对比维度字段")
    private String comparedField;
    @ApiModelProperty(value = "对比维度字段是否为预定义字段")
    private Boolean comparedFieldPredefined;
    @NotBlank(message = "error.customChart.statisticsTypeNotNull")
    @ApiModelProperty(value = "汇总项")
    private String statisticsType;
    @NotBlank(message = "error.customChart.chartTypeNotNull")
    @ApiModelProperty(value = "图表类型", required = true)
    private String chartType;
    @ApiModelProperty(value = "筛选参数")
    private String searchJson;

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public String getAnalysisField() {
        return analysisField;
    }

    public void setAnalysisField(String analysisField) {
        this.analysisField = analysisField;
    }

    public Boolean getAnalysisFieldPredefined() {
        return analysisFieldPredefined;
    }

    public void setAnalysisFieldPredefined(Boolean analysisFieldPredefined) {
        this.analysisFieldPredefined = analysisFieldPredefined;
    }

    public String getComparedField() {
        return comparedField;
    }

    public void setComparedField(String comparedField) {
        this.comparedField = comparedField;
    }

    public Boolean getComparedFieldPredefined() {
        return comparedFieldPredefined;
    }

    public void setComparedFieldPredefined(Boolean comparedFieldPredefined) {
        this.comparedFieldPredefined = comparedFieldPredefined;
    }

    public String getStatisticsType() {
        return statisticsType;
    }

    public void setStatisticsType(String statisticsType) {
        this.statisticsType = statisticsType;
    }

    public String getChartType() {
        return chartType;
    }

    public void setChartType(String chartType) {
        this.chartType = chartType;
    }

    public String getSearchJson() {
        return searchJson;
    }

    public void setSearchJson(String searchJson) {
        this.searchJson = searchJson;
    }
}
