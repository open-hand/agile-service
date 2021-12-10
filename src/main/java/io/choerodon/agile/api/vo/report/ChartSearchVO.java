package io.choerodon.agile.api.vo.report;

import java.util.Date;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonInclude;
import io.choerodon.agile.api.vo.SearchVO;
import org.hzero.starter.keyencrypt.core.Encrypt;

/**
 * @author jiaxu.cui@hand-china.com 2020/9/17 上午10:46
 */
@JsonInclude(JsonInclude.Include.NON_NULL)
public class ChartSearchVO {

    /**
     * 项目id(必选)
     */
    private Long projectId;

    /**
     * 燃尽图： 冲刺id(必选)
     * 冲刺报告图： 冲刺id(必选)
     * 统计图： 冲刺id(可选)
     * 冲刺未完成统计图： 冲刺id(必选)
     * 缺陷提出与解决统计图： 冲刺id(必选)
     * 迭代统计图： 冲刺id(必选)
     * 缺陷累计趋势图： 冲刺id(必选)
     */
    @Encrypt
    private Long sprintId;
    /**
     * 燃尽图： 是否为当前冲刺(可选)
     * 冲刺报告图： 是否为当前冲刺(可选)
     */
    private Boolean currentSprint;
    /**
     * 燃尽图： 类型(必选)
     * 冲刺报告图： 必为issueCount
     * 版本报告图： 类型(必选)
     * 迭代速度图： 类型(必选)
     * 史诗报告图: 类型(必选)
     * 史诗燃耗图： 必为Epic
     * 版本燃耗图： 必为Version
     */
    private String type;
    /**
     * 燃尽图： 仅故事(可选)
     */
    private Boolean onlyStory;
    /**
     * 燃尽图： 快速筛选(可选)
     * 累积流量图： 快速筛选(可选)
     */
    @Encrypt
    private List<Long> quickFilterIds;
    /**
     * 燃尽图： 个人筛选(可选)
     */
    @Encrypt
    private List<Long> personalFilterIds;
    /**
     * 燃尽图： 经办人Id(可选)
     */
    @Encrypt
    private Long assigneeId;
    /**
     * 燃尽图： displayNonWorkingDay(可选)
     * 冲刺报告图: displayNonWorkingDay(可选)
     */
    private Boolean displayNonWorkingDay;

    /**
     * 累积流量图：面板id(必选)
     */
    @Encrypt
    private Long boardId;
    /**
     * 累积流量图：列id(必选)
     */
    @Encrypt
    private List<Long> columnIds;
    /**
     * 累积流量图：起始时间(必选)
     */
    private Date startDate;
    /**
     * 累积流量图：结束时间(必选)
     */
    private Date endDate;
    
    /**
     * 版本报告图: 版本id(必选)
     * 版本燃耗图: 版本id(必选)
     * 统计图： 版本id(可选)
     */
    @Encrypt
    private Long versionId;

    /**
     * 统计图： 状态id(可选)
     */
    @Encrypt
    private Long statusId;

    /**
     * 史诗报告图: 史诗id(必选)
     * 史诗燃耗图： 史诗id(必选)
     */
    @Encrypt
    private Long epicId;

    /**
     * 版本燃耗图: 根据图表校准冲刺(可选)
     * 史诗燃耗图： 根据图表校准冲刺(可选)
     */
    private Boolean calibrationSprint;

    /**
     * 统计图：组织id(必选)
     */
    private Long organizationId;
    /**
     * 统计图：统计类型(必选)
     */
    private String fieldName;

    /**
     * 特性进度图： piId(必填)
     * 子项目工作量： piId(必填)
     */
    @Encrypt
    private Long piId;

    /**
     * 子项目燃尽图：子项目id(必填)
     */
    private Long subProjectId;

    /**
     * 与其余图表的type字段意义等同
     * 子项目工作量： 单位(必填)
     */
    private String latitude;

    private SearchVO currentSearchVO;

    /**
     * 代码质量图近多少天
     */
    private Integer days;

    /**
     * 服务id
     */
    @Encrypt
    private Long serviceId;

    /**
     * 自定义报表类型
     */
    private String chartType;

    /**
     * 自定义报表统计数据类型
     */
    private String statisticsType;

    /**
     * 自定义报表统计字段
     */
    private String analysisField;

    /**
     * 自定义报表对比字段
     */
    private String comparedField;

    /**
     * 自定义报表统计字段是否系统字段
     */
    private Boolean analysisFieldPredefined;

    /**
     * 自定义报表对比字段是否系统字段
     */
    private Boolean comparedFieldPredefined;

    /**
     * 自定义报表查询参数
     */
    private SearchVO searchVO;


    /**
     * 需求统计图维度
     */

    private BacklogPieChartDimensionVO dimension;

    public Long getServiceId() {
        return serviceId;
    }

    public void setServiceId(Long serviceId) {
        this.serviceId = serviceId;
    }

    public Integer getDays() {
        return days;
    }

    public void setDays(Integer days) {
        this.days = days;
    }

    public SearchVO getCurrentSearchVO() {
        return currentSearchVO;
    }

    public void setCurrentSearchVO(SearchVO currentSearchVO) {
        this.currentSearchVO = currentSearchVO;
    }

    public Boolean getCurrentSprint() {
        return currentSprint;
    }

    public void setCurrentSprint(Boolean currentSprint) {
        this.currentSprint = currentSprint;
    }

    public Long getProjectId() {
        return projectId;
    }

    public void setProjectId(Long projectId) {
        this.projectId = projectId;
    }

    public Long getSprintId() {
        return sprintId;
    }

    public void setSprintId(Long sprintId) {
        this.sprintId = sprintId;
    }

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    public Boolean getOnlyStory() {
        return onlyStory;
    }

    public void setOnlyStory(Boolean onlyStory) {
        this.onlyStory = onlyStory;
    }

    public List<Long> getQuickFilterIds() {
        return quickFilterIds;
    }

    public void setQuickFilterIds(List<Long> quickFilterIds) {
        this.quickFilterIds = quickFilterIds;
    }

    public List<Long> getPersonalFilterIds() {
        return personalFilterIds;
    }

    public void setPersonalFilterIds(List<Long> personalFilterIds) {
        this.personalFilterIds = personalFilterIds;
    }

    public Long getAssigneeId() {
        return assigneeId;
    }

    public void setAssigneeId(Long assigneeId) {
        this.assigneeId = assigneeId;
    }

    public Boolean getDisplayNonWorkingDay() {
        return displayNonWorkingDay;
    }

    public void setDisplayNonWorkingDay(Boolean displayNonWorkingDay) {
        this.displayNonWorkingDay = displayNonWorkingDay;
    }

    public Long getBoardId() {
        return boardId;
    }

    public void setBoardId(Long boardId) {
        this.boardId = boardId;
    }

    public List<Long> getColumnIds() {
        return columnIds;
    }

    public void setColumnIds(List<Long> columnIds) {
        this.columnIds = columnIds;
    }

    public Date getStartDate() {
        return startDate;
    }

    public void setStartDate(Date startDate) {
        this.startDate = startDate;
    }

    public Date getEndDate() {
        return endDate;
    }

    public void setEndDate(Date endDate) {
        this.endDate = endDate;
    }

    public Long getVersionId() {
        return versionId;
    }

    public void setVersionId(Long versionId) {
        this.versionId = versionId;
    }

    public Long getEpicId() {
        return epicId;
    }

    public void setEpicId(Long epicId) {
        this.epicId = epicId;
    }

    public Boolean getCalibrationSprint() {
        return calibrationSprint;
    }

    public void setCalibrationSprint(Boolean calibrationSprint) {
        this.calibrationSprint = calibrationSprint;
    }

    public Long getOrganizationId() {
        return organizationId;
    }

    public void setOrganizationId(Long organizationId) {
        this.organizationId = organizationId;
    }

    public String getFieldName() {
        return fieldName;
    }

    public void setFieldName(String fieldName) {
        this.fieldName = fieldName;
    }

    public Long getPiId() {
        return piId;
    }

    public void setPiId(Long piId) {
        this.piId = piId;
    }

    public Long getSubProjectId() {
        return subProjectId;
    }

    public void setSubProjectId(Long subProjectId) {
        this.subProjectId = subProjectId;
    }

    public String getLatitude() {
        return latitude;
    }

    public void setLatitude(String latitude) {
        this.latitude = latitude;
    }

    public Long getStatusId() {
        return statusId;
    }

    public void setStatusId(Long statusId) {
        this.statusId = statusId;
    }

    public String getChartType() {
        return chartType;
    }

    public void setChartType(String chartType) {
        this.chartType = chartType;
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

    public SearchVO getSearchVO() {
        return searchVO;
    }

    public void setSearchVO(SearchVO searchVO) {
        this.searchVO = searchVO;
    }

    public BacklogPieChartDimensionVO getDimension() {
        return dimension;
    }

    public void setDimension(BacklogPieChartDimensionVO dimension) {
        this.dimension = dimension;
    }
}
