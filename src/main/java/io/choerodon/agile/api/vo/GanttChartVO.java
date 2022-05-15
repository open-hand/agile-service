package io.choerodon.agile.api.vo;

import io.choerodon.agile.api.vo.business.ProductVO;
import io.choerodon.agile.api.vo.business.TagVO;
import io.choerodon.agile.api.vo.waterfall.GanttParentVO;
import io.choerodon.agile.api.vo.waterfall.WaterfallIssueVO;
import io.choerodon.agile.infra.dto.IssueSprintDTO;
import io.choerodon.agile.infra.dto.UserMessageDTO;
import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

import java.math.BigDecimal;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * @author superlee
 * @since 2020-11-25
 */
public class GanttChartVO {

    public static final String FIELD_ISSUE_ID ="issueId";
    public static final String FIELD_SUMMARY ="summary";
    public static final String FIELD_ASSIGNEE ="assignee";
    public static final String FIELD_ESTIMATED_START_TIME ="estimatedStartTime";
    public static final String FIELD_ESTIMATED_END_TIME ="estimatedEndTime";
    public static final String FIELD_OBJECT_VERSION_NUMBER ="objectVersionNumber";
    public static final String FIELD_ISSUE_TYPE_VO ="issueTypeVO";
    public static final String FIELD_PARENT_ID ="parentId";

    @Encrypt
    @ApiModelProperty(value = "问题id")
    private Long issueId;
    @ApiModelProperty(value = "问题编码")
    private String issueNum;
    @ApiModelProperty(value = "概要")
    private String summary;
    @ApiModelProperty(value = "经办人")
    private UserMessageDTO assignee;
    @ApiModelProperty(value = "预计开始时间")
    private Date estimatedStartTime;
    @ApiModelProperty(value = "预计结束时间")
    private Date estimatedEndTime;
    @ApiModelProperty(value = "乐观锁")
    private Long objectVersionNumber;
    @ApiModelProperty(value = "问题类型")
    private IssueTypeVO issueTypeVO;
    @ApiModelProperty(value = "优先级")
    private PriorityVO priorityVO;

    @Encrypt
    @ApiModelProperty(value = "父级")
    private Long parentId;
    @ApiModelProperty(value = "状态")
    private StatusVO statusVO;
    @ApiModelProperty(value = "是否已完成")
    private Boolean completed;
    @ApiModelProperty(value = "实际完成时间")
    private Date actualCompletedDate;
    @ApiModelProperty(value = "冲刺")
    private IssueSprintDTO sprint;
    @ApiModelProperty(value = "实际开始时间")
    private Date actualStartTime;
    @ApiModelProperty(value = "实际结束时间")
    private Date actualEndTime;
    @ApiModelProperty(value = "史诗名称")
    private String epicName;
    @ApiModelProperty(value = "颜色")
    private String color;
    @ApiModelProperty(value = "项目群id")
    private Long programId;
    @ApiModelProperty(value = "项目id")
    private Long projectId;
    @Encrypt
    @ApiModelProperty(value = "特性id")
    private Long featureId;
    @Encrypt
    @ApiModelProperty(value = "史诗id")
    private Long epicId;
    @ApiModelProperty(value = "特性名称")
    private String featureName;
    @ApiModelProperty(value = "特性颜色")
    private String featureColor;

    @ApiModelProperty(value = "创建时间")
    private Date creationDate;
    @ApiModelProperty(value = "故事点")
    private BigDecimal storyPoints;

    @ApiModelProperty(value = "最后更新时间")
    private Date lastUpdateDate;

    @ApiModelProperty(value = "预估时间")
    private BigDecimal estimateTime;

    @ApiModelProperty(value = "剩余时间")
    private BigDecimal remainingTime;

    @ApiModelProperty(value = "修复的版本")
    private List<VersionIssueRelVO> fixVersion;

    @ApiModelProperty(value = "影响的版本")
    private List<VersionIssueRelVO> influenceVersion;

    @ApiModelProperty(value = "标签")
    private List<LabelIssueRelVO> labels;

    @ApiModelProperty(value = "模块")
    private List<IssueComponentBriefVO> components;

    @ApiModelProperty(value = "tag")
    private List<TagVO> tags;

    @ApiModelProperty(value = "创建人")
    private UserMessageDTO createUser;

    @ApiModelProperty(value = "更新人")
    private UserMessageDTO updateUser;

    @ApiModelProperty(value = "报告人")
    private UserMessageDTO reporter;

    @ApiModelProperty(value = "主要负责人")
    private UserMessageDTO mainResponsibleUser;

    @ApiModelProperty(value = "自定义字段的kv")
    private Map<String,Object> foundationFieldValue;

    @ApiModelProperty(value = "冲刺")
    private List<IssueSprintVO> sprints;

    @ApiModelProperty(value = "环境")
    private String environment;
    @ApiModelProperty(value = "特性类型")
    private String featureType;

    @ApiModelProperty(value = "参与人")
    private List<UserMessageDTO> participants;
    @ApiModelProperty(value = "项目")
    private ProjectVO project;
    @ApiModelProperty(value = "子项目id集")
    private Set<Long> subProjectIds;
    @ApiModelProperty(value = "工时百分比")
    private BigDecimal workTimePercentage;
    @ApiModelProperty(value = "消耗的工时")
    private BigDecimal spentWorkTime;
    @ApiModelProperty(value = "全部预计时间")
    private BigDecimal allEstimateTime;
    @ApiModelProperty(value = "前置项")
    private List<GanttChartVO> predecessors;
    @ApiModelProperty(value = "前置项类型")
    private String predecessorType;
    @ApiModelProperty(value = "瀑布问题")
    private WaterfallIssueVO waterfallIssueVO;
    @ApiModelProperty(value = "父级")
    private Set<GanttParentVO> parents;
    @Encrypt
    @ApiModelProperty(value = "父级冲刺")
    private Set<Long> parentSprintIds;
    @ApiModelProperty(value = "应用类型")
    private String applyType;
    @ApiModelProperty(value = "产品")
    private List<ProductVO> productVOList;

    public Set<Long> getParentSprintIds() {
        return parentSprintIds;
    }

    public void setParentSprintIds(Set<Long> parentSprintIds) {
        this.parentSprintIds = parentSprintIds;
    }

    public Set<GanttParentVO> getParents() {
        return parents;
    }

    public void setParents(Set<GanttParentVO> parents) {
        this.parents = parents;
    }

    public String getPredecessorType() {
        return predecessorType;
    }

    public void setPredecessorType(String predecessorType) {
        this.predecessorType = predecessorType;
    }

    public List<GanttChartVO> getPredecessors() {
        return predecessors;
    }

    public void setPredecessors(List<GanttChartVO> predecessors) {
        this.predecessors = predecessors;
    }

    public BigDecimal getWorkTimePercentage() {
        return workTimePercentage;
    }

    public void setWorkTimePercentage(BigDecimal workTimePercentage) {
        this.workTimePercentage = workTimePercentage;
    }

    public Set<Long> getSubProjectIds() {
        return subProjectIds;
    }

    public void setSubProjectIds(Set<Long> subProjectIds) {
        this.subProjectIds = subProjectIds;
    }

    public ProjectVO getProject() {
        return project;
    }

    public void setProject(ProjectVO project) {
        this.project = project;
    }

    public String getFeatureType() {
        return featureType;
    }

    public void setFeatureType(String featureType) {
        this.featureType = featureType;
    }

    public Long getIssueId() {
        return issueId;
    }

    public void setIssueId(Long issueId) {
        this.issueId = issueId;
    }

    public String getIssueNum() {
        return issueNum;
    }

    public void setIssueNum(String issueNum) {
        this.issueNum = issueNum;
    }

    public String getSummary() {
        return summary;
    }

    public void setSummary(String summary) {
        this.summary = summary;
    }

    public UserMessageDTO getAssignee() {
        return assignee;
    }

    public void setAssignee(UserMessageDTO assignee) {
        this.assignee = assignee;
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

    public Long getObjectVersionNumber() {
        return objectVersionNumber;
    }

    public void setObjectVersionNumber(Long objectVersionNumber) {
        this.objectVersionNumber = objectVersionNumber;
    }

    public IssueTypeVO getIssueTypeVO() {
        return issueTypeVO;
    }

    public void setIssueTypeVO(IssueTypeVO issueTypeVO) {
        this.issueTypeVO = issueTypeVO;
    }

    public Long getParentId() {
        return parentId;
    }

    public void setParentId(Long parentId) {
        this.parentId = parentId;
    }

    public StatusVO getStatusVO() {
        return statusVO;
    }

    public void setStatusVO(StatusVO statusVO) {
        this.statusVO = statusVO;
    }

    public Boolean getCompleted() {
        return completed;
    }

    public void setCompleted(Boolean completed) {
        this.completed = completed;
    }

    public Date getActualCompletedDate() {
        return actualCompletedDate;
    }

    public void setActualCompletedDate(Date actualCompletedDate) {
        this.actualCompletedDate = actualCompletedDate;
    }

    public IssueSprintDTO getSprint() {
        return sprint;
    }

    public void setSprint(IssueSprintDTO sprint) {
        this.sprint = sprint;
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

    public String getEpicName() {
        return epicName;
    }

    public void setEpicName(String epicName) {
        this.epicName = epicName;
    }

    public String getColor() {
        return color;
    }

    public void setColor(String color) {
        this.color = color;
    }

    public Long getProgramId() {
        return programId;
    }

    public void setProgramId(Long programId) {
        this.programId = programId;
    }

    public Long getProjectId() {
        return projectId;
    }

    public void setProjectId(Long projectId) {
        this.projectId = projectId;
    }

    public Long getFeatureId() {
        return featureId;
    }

    public void setFeatureId(Long featureId) {
        this.featureId = featureId;
    }

    public Long getEpicId() {
        return epicId;
    }

    public void setEpicId(Long epicId) {
        this.epicId = epicId;
    }

    public String getFeatureName() {
        return featureName;
    }

    public void setFeatureName(String featureName) {
        this.featureName = featureName;
    }

    public Date getCreationDate() {
        return creationDate;
    }

    public void setCreationDate(Date creationDate) {
        this.creationDate = creationDate;
    }

    public BigDecimal getStoryPoints() {
        return storyPoints;
    }

    public void setStoryPoints(BigDecimal storyPoints) {
        this.storyPoints = storyPoints;
    }

    public Date getLastUpdateDate() {
        return lastUpdateDate;
    }

    public void setLastUpdateDate(Date lastUpdateDate) {
        this.lastUpdateDate = lastUpdateDate;
    }

    public BigDecimal getEstimateTime() {
        return estimateTime;
    }

    public void setEstimateTime(BigDecimal estimateTime) {
        this.estimateTime = estimateTime;
    }

    public BigDecimal getRemainingTime() {
        return remainingTime;
    }

    public void setRemainingTime(BigDecimal remainingTime) {
        this.remainingTime = remainingTime;
    }

    public List<VersionIssueRelVO> getFixVersion() {
        return fixVersion;
    }

    public void setFixVersion(List<VersionIssueRelVO> fixVersion) {
        this.fixVersion = fixVersion;
    }

    public List<VersionIssueRelVO> getInfluenceVersion() {
        return influenceVersion;
    }

    public void setInfluenceVersion(List<VersionIssueRelVO> influenceVersion) {
        this.influenceVersion = influenceVersion;
    }

    public List<LabelIssueRelVO> getLabels() {
        return labels;
    }

    public void setLabels(List<LabelIssueRelVO> labels) {
        this.labels = labels;
    }

    public List<IssueComponentBriefVO> getComponents() {
        return components;
    }

    public void setComponents(List<IssueComponentBriefVO> components) {
        this.components = components;
    }

    public List<TagVO> getTags() {
        return tags;
    }

    public void setTags(List<TagVO> tags) {
        this.tags = tags;
    }

    public UserMessageDTO getCreateUser() {
        return createUser;
    }

    public void setCreateUser(UserMessageDTO createUser) {
        this.createUser = createUser;
    }

    public UserMessageDTO getUpdateUser() {
        return updateUser;
    }

    public void setUpdateUser(UserMessageDTO updateUser) {
        this.updateUser = updateUser;
    }

    public UserMessageDTO getMainResponsibleUser() {
        return mainResponsibleUser;
    }

    public void setMainResponsibleUser(UserMessageDTO mainResponsibleUser) {
        this.mainResponsibleUser = mainResponsibleUser;
    }

    public Map<String, Object> getFoundationFieldValue() {
        return foundationFieldValue;
    }

    public void setFoundationFieldValue(Map<String, Object> foundationFieldValue) {
        this.foundationFieldValue = foundationFieldValue;
    }

    public List<IssueSprintVO> getSprints() {
        return sprints;
    }

    public void setSprints(List<IssueSprintVO> sprints) {
        this.sprints = sprints;
    }

    public String getEnvironment() {
        return environment;
    }

    public void setEnvironment(String environment) {
        this.environment = environment;
    }

    public PriorityVO getPriorityVO() {
        return priorityVO;
    }

    public void setPriorityVO(PriorityVO priorityVO) {
        this.priorityVO = priorityVO;
    }

    public List<UserMessageDTO> getParticipants() {
        return participants;
    }

    public void setParticipants(List<UserMessageDTO> participants) {
        this.participants = participants;
    }

    public UserMessageDTO getReporter() {
        return reporter;
    }

    public void setReporter(UserMessageDTO reporter) {
        this.reporter = reporter;
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

    public WaterfallIssueVO getWaterfallIssueVO() {
        return waterfallIssueVO;
    }

    public void setWaterfallIssueVO(WaterfallIssueVO waterfallIssueVO) {
        this.waterfallIssueVO = waterfallIssueVO;
    }

    public String getApplyType() {
        return applyType;
    }

    public void setApplyType(String applyType) {
        this.applyType = applyType;
    }

    public String getFeatureColor() {
        return featureColor;
    }

    public void setFeatureColor(String featureColor) {
        this.featureColor = featureColor;
    }

    public List<ProductVO> getProductVOList() {
        return productVOList;
    }

    public void setProductVOList(List<ProductVO> productVOList) {
        this.productVOList = productVOList;
    }
}