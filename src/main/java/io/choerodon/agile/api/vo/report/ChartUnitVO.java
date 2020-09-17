package io.choerodon.agile.api.vo.report;

/**
 * @author jiaxu.cui@hand-china.com 2020/9/15 下午7:01
 */
public class ChartUnitVO extends ReportUnitVO {

    /**
     * 图表代码
     */
    private String chartCode;
    /**
     * 图表查询参数
     */
    private ChartSearchVO chartSearchVO;

    public String getChartCode() {
        return chartCode;
    }

    public void setChartCode(String chartCode) {
        this.chartCode = chartCode;
    }

    public ChartSearchVO getChartSearchVO() {
        return chartSearchVO;
    }

    public void setChartSearchVO(ChartSearchVO chartSearchVO) {
        this.chartSearchVO = chartSearchVO;
    }
}
