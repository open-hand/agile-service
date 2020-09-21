package io.choerodon.agile.api.vo.report;

import org.apache.commons.lang3.StringUtils;
import org.hzero.core.base.BaseConstants;
import org.springframework.util.Assert;

/**
 * @author jiaxu.cui@hand-china.com 2020/9/15 下午7:01
 */
public class ChartUnitVO extends ReportUnitVO {
    
    public static final String CHART_TYPE_ISSUE_COUNT = "issueCount";
    public static final String CHART_TYPE_EPIC = "Epic";
    public static final String CHART_TYPE_VERSION = "Version";

    /**
     * 燃尽图
     */
    public static final String BURN_DOWN_REPORT = "burn_down_report";
    /**
     * 冲刺报告图
     */
    public static final String SPRINT_REPORT = "sprint_report";
    /**
     * 累积流量图
     */
    public static final String CUMULATIVE_FLOW_DIAGRAM = "cumulative_flow_diagram";   
    /**
     * 版本报告图
     */
    public static final String VERSION_CHART = "version_chart";    
    /**
     * 迭代速度图
     */
    public static final String VELOCITY_CHART = "velocity_chart";
    /**
     * 史诗报告图
     */
    public static final String EPIC_CHART = "epic_chart";
    /**
     * 统计图
     */
    public static final String PIE_CHART = "pie_chart";
    /**
     * 史诗燃耗图
     */
    public static final String EPIC_BURN_DOWN_REPORT = "epic_burn_down_report";
    /** 
     * 版本燃耗图
     */
    public static final String VERSION_BURN_DOWN_REPORT = "version_burn_down_report";
    /**
     * 冲刺未完成统计图
     */
    public static final String UNCOMPLETED_CHART = "uncompleted_chart";
    /**
     * 缺陷提出与解决统计图
     */
    public static final String ISSUE_CHART = "issue_chart";
    /**
     * 迭代统计图
     */
    public static final String SPRINT_STATISTICS_CHART = "sprint_statistics_chart";
    /**
     * 缺陷累计趋势图
     */
    public static final String ISSUE_COUNT_CHART = "issue_count_chart";

    /**
     * 各图表必输校验
     */
    public void validateAndconvert(){
        super.validateAndconvert();
        ChartSearchVO chartSearchVO = getChartSearchVO();
        Assert.notNull(chartSearchVO, BaseConstants.ErrorCode.DATA_INVALID);
        Assert.notNull(chartSearchVO.getProjectId(), BaseConstants.ErrorCode.DATA_INVALID);

        switch (this.chartCode){
            case BURN_DOWN_REPORT:
                Assert.notNull(chartSearchVO.getSprintId(), BaseConstants.ErrorCode.DATA_INVALID);
                Assert.notNull(chartSearchVO.getType(), BaseConstants.ErrorCode.DATA_INVALID);
                break;
            case SPRINT_REPORT:
                Assert.notNull(chartSearchVO.getSprintId(), BaseConstants.ErrorCode.DATA_INVALID);
                break;
            case CUMULATIVE_FLOW_DIAGRAM:
                Assert.notNull(chartSearchVO.getBoardId(), BaseConstants.ErrorCode.DATA_INVALID);
                Assert.notNull(chartSearchVO.getStartDate(), BaseConstants.ErrorCode.DATA_INVALID);
                Assert.notNull(chartSearchVO.getEndDate(), BaseConstants.ErrorCode.DATA_INVALID);
                break;
            case VERSION_CHART:
                Assert.notNull(chartSearchVO.getType(), BaseConstants.ErrorCode.DATA_INVALID);
                Assert.notNull(chartSearchVO.getVersionId(), BaseConstants.ErrorCode.DATA_INVALID);
                break;
            case VELOCITY_CHART:
                Assert.notNull(chartSearchVO.getType(), BaseConstants.ErrorCode.DATA_INVALID);
                break;
            case EPIC_CHART:
                Assert.notNull(chartSearchVO.getType(), BaseConstants.ErrorCode.DATA_INVALID);
                Assert.notNull(chartSearchVO.getEpicId(), BaseConstants.ErrorCode.DATA_INVALID);
                break;
            case PIE_CHART:
                Assert.notNull(chartSearchVO.getOrganizationId(), BaseConstants.ErrorCode.DATA_INVALID);
                Assert.notNull(chartSearchVO.getFieldName(), BaseConstants.ErrorCode.DATA_INVALID);
                break;
            case EPIC_BURN_DOWN_REPORT:
                Assert.notNull(chartSearchVO.getEpicId(), BaseConstants.ErrorCode.DATA_INVALID);
                break;
            case VERSION_BURN_DOWN_REPORT:
                break;
            case UNCOMPLETED_CHART:
            case ISSUE_CHART:
            case SPRINT_STATISTICS_CHART:
            case ISSUE_COUNT_CHART:
                Assert.notNull(chartSearchVO.getSprintId(), BaseConstants.ErrorCode.DATA_INVALID);
                break;
            default:
                break;
        }
    }

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
