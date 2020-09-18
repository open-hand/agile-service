package io.choerodon.agile.api.vo.report;

import java.util.Date;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonInclude;
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
     * 统计图： 版本id(可选)
     */
    @Encrypt
    private Long versionId;

    /**
     * 史诗报告图: 史诗id(必选)
     * 史诗燃耗图： 史诗id(必选)
     */
    @Encrypt
    private Long epicId;

    /**
     * 统计图：组织id(必选)
     */
    private Long organizationId;
    /**
     * 统计图：统计类型(必选)
     */
    private String fieldName;

    public List<Long> getPersonalFilterIds() {
        return personalFilterIds;
    }

    public void setPersonalFilterIds(List<Long> personalFilterIds) {
        this.personalFilterIds = personalFilterIds;
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
}
