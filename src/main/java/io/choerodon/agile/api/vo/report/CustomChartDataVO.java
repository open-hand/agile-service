package io.choerodon.agile.api.vo.report;

import java.util.List;

/**
 * @author chihao.ran@hand-china.com
 * 2021/06/11 10:34
 */
public class CustomChartDataVO {
    private List<CustomChartDimensionVO> dimensionList;
    private String chartType;

    public String getChartType() {
        return chartType;
    }

    public void setChartType(String chartType) {
        this.chartType = chartType;
    }

    public List<CustomChartDimensionVO> getDimensionList() {
        return dimensionList;
    }

    public void setDimensionList(List<CustomChartDimensionVO> dimensionList) {
        this.dimensionList = dimensionList;
    }
}
