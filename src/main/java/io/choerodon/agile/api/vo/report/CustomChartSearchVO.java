package io.choerodon.agile.api.vo.report;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;

import io.choerodon.agile.api.vo.SearchVO;
import io.choerodon.agile.infra.utils.StringUtil;

/**
 * @author chihao.ran@hand-china.com
 * 2021/06/11 10:34
 */
public class CustomChartSearchVO {
    private SearchVO searchVO;
    @NotBlank(message = "error.customChart.statisticsTypeNotNull")
    private String statisticsType;
    @NotBlank(message = "error.customChart.analysisFieldNotNull")
    private String analysisField;
    @NotNull(message = "error.customChart.analysisFieldPredefinedNotNull")
    private Boolean analysisFieldPredefined;

    private String comparedField;

    private Boolean comparedFieldPredefined;

    @NotBlank(message = "error.customChart.chartTypeNotNull")
    private String chartType;

    public SearchVO getSearchVO() {
        return searchVO;
    }

    public void setSearchVO(SearchVO searchVO) {
        this.searchVO = searchVO;
    }

    public String getStatisticsType() {
        return statisticsType;
    }

    public void setStatisticsType(String statisticsType) {
        this.statisticsType = statisticsType;
    }

    public String getAnalysisField() {
        return analysisField;
    }

    public void setAnalysisField(String analysisField) {
        this.analysisField = analysisField;
    }

    public String getComparedField() {
        return comparedField;
    }

    public void setComparedField(String comparedField) {
        this.comparedField = comparedField;
    }


    public String getChartType() {
        return chartType;
    }

    public void setChartType(String chartType) {
        this.chartType = chartType;
    }

    public Boolean getAnalysisFieldPredefined() {
        return analysisFieldPredefined;
    }

    public void setAnalysisFieldPredefined(Boolean analysisFieldPredefined) {
        this.analysisFieldPredefined = analysisFieldPredefined;
    }

    public Boolean getComparedFieldPredefined() {
        return comparedFieldPredefined;
    }

    public void setComparedFieldPredefined(Boolean comparedFieldPredefined) {
        this.comparedFieldPredefined = comparedFieldPredefined;
    }

    @Override
    public String toString() {
        return StringUtil.getToString(this);
    }
}
