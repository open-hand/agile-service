package io.choerodon.agile.infra.enums;

/**
 * @author huaxin.deng@hand-china.com 2021-11-16 16:38:07
 */
public class PersonalFilterTypeCode {
    private PersonalFilterTypeCode() {
    }

    /**
     * 工作项个人筛选
     */
    public static final String AGILE_ISSUE = "agile_issue";

    /**
     * 工时日历个人筛选
     */
    public static final String AGILE_WORK_HOURS = "agile_work_hours";

    /**
     * 风险个人筛选
     */
    public static final String RISK_ISSUE = "risk_issue";
    /**
     * 特性个人筛选
     */
    public static final String FEATURE_ISSUE = "feature_issue";
    /**
     * 瀑布个人筛选
     */
    public static final String WATERFALL_ISSUE = "waterfall_issue";
    /**
     * 问题分析
     */
    public static final String ISSUE_ANALYSIS = "issue_analysis";
    /**
     * 缺陷分析
     */
    public static final String BUG_ANALYSIS = "bug_analysis";
    /**
     * 需求分析
     */
    public static final String BACKLOG_ANALYSIS = "backlog_analysis";

}
