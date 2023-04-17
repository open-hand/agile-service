package io.choerodon.agile.api.vo;

import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.annotations.ApiModelProperty;

import io.choerodon.agile.api.vo.report.CustomChartDataVO;

import org.hzero.starter.keyencrypt.core.Encrypt;

/**
 * @author chihao.ran@hand-china.com
 * 2021/06/21 16:40
 */
@JsonInclude(JsonInclude.Include.NON_NULL)
public class CustomChartVO {

    @Encrypt
    @ApiModelProperty(value = "主键id")
    private Long id;
    @ApiModelProperty(value = "自定义报表名称")
    private String name;
    @ApiModelProperty(value = "描述")
    private String description;
    @ApiModelProperty(value = "分析维度字段")
    private String analysisField;
    @ApiModelProperty(value = "分析维度字段是否为预定义字段")
    private Boolean analysisFieldPredefined;
    @ApiModelProperty(value = "对比维度字段")
    private String comparedField;
    @ApiModelProperty(value = "对比维度字段是否为预定义字段")
    private Boolean comparedFieldPredefined;
    @ApiModelProperty(value = "汇总项")
    private String statisticsType;
    @ApiModelProperty(value = "图表类型")
    private String chartType;
    @ApiModelProperty(value = "筛选参数")
    private String searchJson;
    @ApiModelProperty(value = "自定义报表图表数据")
    private CustomChartDataVO customChartData;
    @ApiModelProperty(value = "版本号")
    private Long objectVersionNumber;

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

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

    public Long getObjectVersionNumber() {
        return objectVersionNumber;
    }

    public void setObjectVersionNumber(Long objectVersionNumber) {
        this.objectVersionNumber = objectVersionNumber;
    }

    public CustomChartDataVO getCustomChartData() {
        return customChartData;
    }

    public void setCustomChartData(CustomChartDataVO customChartData) {
        this.customChartData = customChartData;
    }
}
