package io.choerodon.agile.api.vo.business;

import io.swagger.annotations.ApiModelProperty;

import java.math.BigDecimal;
import java.util.Date;
import java.util.Map;

public class ExportIssuesVO {

    public static final String SUMMARY = "summary";
    public static final String ISSUE_NUM = "issueNum";
    public static final String EPIC_NAME = "epicName";
    public static final String TYPE_NAME = "typeName";
    public static final String DESCRIPTION = "description";
    public static final String PRIORITY_NAME = "priorityName";
    public static final String STATUS_NAME = "statusName";
    public static final String RESOLUTION = "resolution";
    public static final String SPRINT_NAME = "sprintName";
    public static final String ASSIGNEE_NAME = "assigneeName";
    public static final String REPORTER_NAME = "reporterName";
    public static final String STORY_POINTS = "storyPoints";
    public static final String REMAINING_TIME = "remainingTime";
    public static final String ESTIMATE_TIME = "estimateTime";
    public static final String VERSION_NAME = "versionName";
    public static final String FIX_VERSION_NAME = "fixVersionName";
    public static final String INFLUENCE_VERSION_NAME = "influenceVersionName";
    public static final String LABEL_NAME = "labelName";
    public static final String COMPONENT_NAME = "componentName";
    public static final String CREATION_DATE = "creationDate";
    public static final String LAST_UPDATE_DATE = "lastUpdateDate";
    public static final String ESTIMATED_START_TIME = "estimatedStartTime";
    public static final String ACTUAL_START_TIME = "actualStartTime";
    public static final String ESTIMATED_END_TIME = "estimatedEndTime";
    public static final String ACTUAL_END_TIME = "actualEndTime";
    public static final String CREATED_USER_NAME = "createdUserName";
    public static final String LAST_UPDATE_USER_NAME = "lastUpdatedUserName";
    public static final String MAIN_RESPONSIBLE_NAME = "mainResponsibleName";
    public static final String ENVIRONMENT_NAME = "environmentName";
    public static final String SPENT_WORK_TIME = "spentWorkTime";
    public static final String ALL_ESTIMATE_TIME = "allEstimateTime";
    public static final String TAGS = "tags";
    public static final String RELATED_ISSUE = "relatedIssue";
    public static final String EPIC_SELF_NAME = "epicSelfName";
    public static final String PARTICIPANT = "participant";
    public static final String PRODUCT = "product";
    public static final String FEATURE = "feature";
    public static final String PROGRESS = "progress";
    public static final String SEQUENCE = "sequence";



    @ApiModelProperty("问题id")
    private Long issueId;
    @ApiModelProperty("问题编号")
    private String issueNum;
    @ApiModelProperty("未完成的问题列表")
    private String summary;
    @ApiModelProperty("问题类型名称")
    private String typeName;
    @ApiModelProperty("项目编码")
    private String projectCode;
    @ApiModelProperty("经办人id")
    private Long assigneeId;
    @ApiModelProperty("经办人名称")
    private String assigneeName;
    @ApiModelProperty("经办人真实名称")
    private String assigneeRealName;
    @ApiModelProperty("报告人id")
    private Long reporterId;
    @ApiModelProperty("报告人名称")
    private String reporterName;
    @ApiModelProperty("报告人真实名称")
    private String reporterRealName;
    @ApiModelProperty("状态名称")
    private String statusName;
    @ApiModelProperty("描述")
    private String description;
    @ApiModelProperty("冲刺名称")
    private String sprintName;
    @ApiModelProperty("关闭冲刺的名称")
    private String closeSprintName;
    @ApiModelProperty("创建时间")
    private Date creationDate;
    @ApiModelProperty("最后更新时间")
    private Date lastUpdateDate;
    @ApiModelProperty("优先级名称")
    private String priorityName;
    @ApiModelProperty("子任务")
    private String subTask;
    @ApiModelProperty("预估时间")
    private BigDecimal estimateTime;
    @ApiModelProperty("剩余时间")
    private BigDecimal remainingTime;
    @ApiModelProperty("修复版本名称")
    private String fixVersionName;
    @ApiModelProperty("影响版本名称")
    private String influenceVersionName;
    @ApiModelProperty("项目名称")
    private String projectName;
    @ApiModelProperty("修复版本和影响版本的名称")
    private String versionName;
    @ApiModelProperty("solution")
    private String solution;
    @ApiModelProperty("预估时间总和")
    private BigDecimal sumEstimateTime;
    @ApiModelProperty("剩余时间总和")
    private BigDecimal sumRemainingTime;
    @ApiModelProperty("史诗名称")
    private String epicName;
    @ApiModelProperty("特性")
    private String feature;
    @ApiModelProperty("故事点")
    private BigDecimal storyPoints;
    @ApiModelProperty("模块名称")
    private String componentName;
    @ApiModelProperty("标签名称")
    private String labelName;
    @ApiModelProperty("resolution")
    private String resolution;
    @ApiModelProperty("自定义字段kv")
    private Map<String, Object> foundationFieldValue;
    @ApiModelProperty("预计开始时间")
    private Date estimatedStartTime;
    @ApiModelProperty("预计结束时间")
    private Date estimatedEndTime;
    @ApiModelProperty("预计开始时间")
    private Date actualStartTime;
    @ApiModelProperty("预计开始时间")
    private Date actualEndTime;
    private String teamProjects;

    @ApiModelProperty("创建人名称")
    private String createdUserName;

    @ApiModelProperty("创建人真实名称")
    private String createdUserRealName;

    @ApiModelProperty("更新人名称")
    private String lastUpdatedUserName;

    @ApiModelProperty("更新人真实名称")
    private String lastUpdatedUserRealName;

    @ApiModelProperty("主要负责人")
    private String mainResponsibleName;

    @ApiModelProperty("环境")
    private String environmentName;

    @ApiModelProperty("已耗费时间")
    private BigDecimal spentWorkTime;

    @ApiModelProperty("总预估时间")
    private BigDecimal allEstimateTime;
    @ApiModelProperty(value = "标记")
    private String tags;
    @ApiModelProperty(value = "关联的问题")
    private String relatedIssue;
    @ApiModelProperty(value = "关联问题数量")
    private Integer relatedIssueCount = 1;
    @ApiModelProperty(value = "史诗自己的名称")
    private String epicSelfName;
    @ApiModelProperty(value = "参与人")
    private String participant;

    @ApiModelProperty("工时")
    private BigDecimal workTime;

    @ApiModelProperty("历史累计工时")
    private BigDecimal cumulativeWorkTime;

    @ApiModelProperty("偏差率")
    private String deviationRate;
    @ApiModelProperty(value = "项目id")
    private Long projectId;
    @ApiModelProperty(value = "父级id")
    private Long parentId;
    @ApiModelProperty(value = "父级类型")
    private String parentType;
    @ApiModelProperty(value = "是否合并列")
    private Boolean mergeColumn;
    @ApiModelProperty(value = "颜色")
    private Integer colorIndex;
    @ApiModelProperty(value = "产品")
    private String product;
    @ApiModelProperty(value = "类型")
    private String type;

    @ApiModelProperty("进度")
    private String progress;
    @ApiModelProperty(value = "序列")
    private String sequence;

    public ExportIssuesVO() {
    }

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    public String getSequence() {
        return sequence;
    }

    public void setSequence(String sequence) {
        this.sequence = sequence;
    }

    public String getParentType() {
        return parentType;
    }

    public void setParentType(String parentType) {
        this.parentType = parentType;
    }

    public String getProgress() {
        return progress;
    }

    public void setProgress(String progress) {
        this.progress = progress;
    }

    public String getEpicSelfName() {
        return epicSelfName;
    }

    public void setEpicSelfName(String epicSelfName) {
        this.epicSelfName = epicSelfName;
    }

    public Integer getRelatedIssueCount() {
        return relatedIssueCount;
    }

    public void setRelatedIssueCount(Integer relatedIssueCount) {
        this.relatedIssueCount = relatedIssueCount;
    }

    public String getRelatedIssue() {
        return relatedIssue;
    }

    public void setRelatedIssue(String relatedIssue) {
        this.relatedIssue = relatedIssue;
    }

    public String getTags() {
        return tags;
    }

    public void setTags(String tags) {
        this.tags = tags;
    }

    public String getTeamProjects() {
        return teamProjects;
    }

    public void setTeamProjects(String teamProjects) {
        this.teamProjects = teamProjects;
    }

    public String getFeature() {
        return feature;
    }

    public void setFeature(String feature) {
        this.feature = feature;
    }

    public String getAssigneeRealName() {
        return this.assigneeRealName;
    }

    public void setAssigneeRealName(String assigneeRealName) {
        this.assigneeRealName = assigneeRealName;
    }

    public String getReporterRealName() {
        return this.reporterRealName;
    }

    public void setReporterRealName(String reporterRealName) {
        this.reporterRealName = reporterRealName;
    }

    public String getResolution() {
        return this.resolution;
    }

    public void setResolution(String resolution) {
        this.resolution = resolution;
    }

    public String getProjectName() {
        return this.projectName;
    }

    public void setProjectName(String projectName) {
        this.projectName = projectName;
    }

    public String getVersionName() {
        return this.versionName;
    }

    public void setVersionName(String versionName) {
        this.versionName = versionName;
    }

    public Long getIssueId() {
        return this.issueId;
    }

    public void setIssueId(Long issueId) {
        this.issueId = issueId;
    }

    public String getIssueNum() {
        return this.issueNum;
    }

    public void setIssueNum(String issueNum) {
        this.issueNum = issueNum;
    }

    public String getSummary() {
        return this.summary;
    }

    public void setSummary(String summary) {
        this.summary = summary;
    }

    public String getTypeName() {
        return this.typeName;
    }

    public void setTypeName(String typeName) {
        this.typeName = typeName;
    }

    public String getProjectCode() {
        return this.projectCode;
    }

    public void setProjectCode(String projectCode) {
        this.projectCode = projectCode;
    }

    public Long getAssigneeId() {
        return this.assigneeId;
    }

    public void setAssigneeId(Long assigneeId) {
        this.assigneeId = assigneeId;
    }

    public Long getReporterId() {
        return this.reporterId;
    }

    public void setReporterId(Long reporterId) {
        this.reporterId = reporterId;
    }

    public String getStatusName() {
        return this.statusName;
    }

    public void setStatusName(String statusName) {
        this.statusName = statusName;
    }

    public String getDescription() {
        return this.description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public String getSprintName() {
        return this.sprintName;
    }

    public void setSprintName(String sprintName) {
        this.sprintName = sprintName;
    }

    public Date getCreationDate() {
        return this.creationDate;
    }

    public void setCreationDate(Date creationDate) {
        this.creationDate = creationDate;
    }

    public Date getLastUpdateDate() {
        return this.lastUpdateDate;
    }

    public void setLastUpdateDate(Date lastUpdateDate) {
        this.lastUpdateDate = lastUpdateDate;
    }

    public String getPriorityName() {
        return this.priorityName;
    }

    public void setPriorityName(String priorityName) {
        this.priorityName = priorityName;
    }

    public String getSubTask() {
        return this.subTask;
    }

    public void setSubTask(String subTask) {
        this.subTask = subTask;
    }

    public BigDecimal getEstimateTime() {
        return this.estimateTime;
    }

    public void setEstimateTime(BigDecimal estimateTime) {
        this.estimateTime = estimateTime;
    }

    public BigDecimal getRemainingTime() {
        return this.remainingTime;
    }

    public void setRemainingTime(BigDecimal remainingTime) {
        this.remainingTime = remainingTime;
    }

    public String getCloseSprintName() {
        return this.closeSprintName;
    }

    public void setCloseSprintName(String closeSprintName) {
        this.closeSprintName = closeSprintName;
    }

    public String getFixVersionName() {
        return this.fixVersionName;
    }

    public void setFixVersionName(String fixVersionName) {
        this.fixVersionName = fixVersionName;
    }

    public String getInfluenceVersionName() {
        return this.influenceVersionName;
    }

    public void setInfluenceVersionName(String influenceVersionName) {
        this.influenceVersionName = influenceVersionName;
    }

    public String getAssigneeName() {
        return this.assigneeName;
    }

    public void setAssigneeName(String assigneeName) {
        this.assigneeName = assigneeName;
    }

    public String getReporterName() {
        return this.reporterName;
    }

    public void setReporterName(String reporterName) {
        this.reporterName = reporterName;
    }

    public String getSolution() {
        return this.solution;
    }

    public void setSolution(String solution) {
        this.solution = solution;
    }

    public BigDecimal getSumEstimateTime() {
        return this.sumEstimateTime;
    }

    public void setSumEstimateTime(BigDecimal sumEstimateTime) {
        this.sumEstimateTime = sumEstimateTime;
    }

    public BigDecimal getSumRemainingTime() {
        return this.sumRemainingTime;
    }

    public void setSumRemainingTime(BigDecimal sumRemainingTime) {
        this.sumRemainingTime = sumRemainingTime;
    }

    public String getEpicName() {
        return this.epicName;
    }

    public void setEpicName(String epicName) {
        this.epicName = epicName;
    }

    public BigDecimal getStoryPoints() {
        return this.storyPoints;
    }

    public void setStoryPoints(BigDecimal storyPoints) {
        this.storyPoints = storyPoints;
    }

    public String getComponentName() {
        return this.componentName;
    }

    public void setComponentName(String componentName) {
        this.componentName = componentName;
    }

    public String getLabelName() {
        return this.labelName;
    }

    public void setLabelName(String labelName) {
        this.labelName = labelName;
    }

    public Map<String, Object> getFoundationFieldValue() {
        return this.foundationFieldValue;
    }

    public void setFoundationFieldValue(Map<String, Object> foundationFieldValue) {
        this.foundationFieldValue = foundationFieldValue;
    }

    public Date getEstimatedStartTime() {
        return estimatedStartTime;
    }

    public void setEstimatedStartTime(Date estimatedStartTime) {
        this.estimatedStartTime = estimatedStartTime;
    }

    public Date getEstimatedEndTime() {
        return estimatedEndTime;
    }

    public void setEstimatedEndTime(Date estimatedEndTime) {
        this.estimatedEndTime = estimatedEndTime;
    }

    public String getCreatedUserName() {
        return createdUserName;
    }

    public void setCreatedUserName(String createdUserName) {
        this.createdUserName = createdUserName;
    }

    public String getLastUpdatedUserName() {
        return lastUpdatedUserName;
    }

    public void setLastUpdatedUserName(String lastUpdatedUserName) {
        this.lastUpdatedUserName = lastUpdatedUserName;
    }

    public String getCreatedUserRealName() {
        return createdUserRealName;
    }

    public void setCreatedUserRealName(String createdUserRealName) {
        this.createdUserRealName = createdUserRealName;
    }

    public String getLastUpdatedUserRealName() {
        return lastUpdatedUserRealName;
    }

    public void setLastUpdatedUserRealName(String lastUpdatedUserRealName) {
        this.lastUpdatedUserRealName = lastUpdatedUserRealName;
    }

    public String getMainResponsibleName() {
        return mainResponsibleName;
    }

    public void setMainResponsibleName(String mainResponsibleName) {
        this.mainResponsibleName = mainResponsibleName;
    }

    public String getEnvironmentName() {
        return environmentName;
    }

    public void setEnvironmentName(String environmentName) {
        this.environmentName = environmentName;
    }

    public BigDecimal getSpentWorkTime() {
        return spentWorkTime;
    }

    public void setSpentWorkTime(BigDecimal spentWorkTime) {
        this.spentWorkTime = spentWorkTime;
    }

    public BigDecimal getAllEstimateTime() {
        return allEstimateTime;
    }

    public void setAllEstimateTime(BigDecimal allEstimateTime) {
        this.allEstimateTime = allEstimateTime;
    }

    public Date getActualStartTime() {
        return actualStartTime;
    }

    public void setActualStartTime(Date actualStartTime) {
        this.actualStartTime = actualStartTime;
    }

    public Date getActualEndTime() {
        return actualEndTime;
    }

    public void setActualEndTime(Date actualEndTime) {
        this.actualEndTime = actualEndTime;
    }

    public String getParticipant() {
        return participant;
    }

    public void setParticipant(String participant) {
        this.participant = participant;
    }

    public BigDecimal getWorkTime() {
        return workTime;
    }

    public void setWorkTime(BigDecimal workTime) {
        this.workTime = workTime;
    }

    public BigDecimal getCumulativeWorkTime() {
        return cumulativeWorkTime;
    }

    public void setCumulativeWorkTime(BigDecimal cumulativeWorkTime) {
        this.cumulativeWorkTime = cumulativeWorkTime;
    }

    public String getDeviationRate() {
        return deviationRate;
    }

    public void setDeviationRate(String deviationRate) {
        this.deviationRate = deviationRate;
    }

    public Long getProjectId() {
        return projectId;
    }

    public void setProjectId(Long projectId) {
        this.projectId = projectId;
    }

    public Long getParentId() {
        return parentId;
    }

    public void setParentId(Long parentId) {
        this.parentId = parentId;
    }

    public Boolean getMergeColumn() {
        return mergeColumn;
    }

    public void setMergeColumn(Boolean mergeColumn) {
        this.mergeColumn = mergeColumn;
    }

    public Integer getColorIndex() {
        return colorIndex;
    }

    public void setColorIndex(Integer colorIndex) {
        this.colorIndex = colorIndex;
    }

    public String getProduct() {
        return product;
    }

    public void setProduct(String product) {
        this.product = product;
    }
}
