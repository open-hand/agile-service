package io.choerodon.agile.api.vo.business;

import java.math.BigDecimal;
import java.util.Date;
import java.util.List;
import java.util.Map;

import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.infra.dto.UserMessageDTO;
import io.choerodon.agile.infra.utils.StringUtil;

/**
 * Created by WangZhe@choerodon.io on 2019-06-28.
 * Email: ettwz@hotmail.com
 */
public class IssueListFieldKVVO {

    @ApiModelProperty(value = "问题主键id")
    @Encrypt
    private Long issueId;

    @ApiModelProperty(value = "问题编号")
    private String issueNum;

    @ApiModelProperty(value = "问题类型code")
    private String typeCode;

    @ApiModelProperty(value = "问题概要")
    private String summary;

    @ApiModelProperty(value = "经办人id")
    private Long assigneeId;

    @ApiModelProperty(value = "报告人id")
    private Long reporterId;

    @ApiModelProperty(value = "项目id")
    private Long projectId;

    @ApiModelProperty(value = "问题类型id")
    @Encrypt
    private Long issueTypeId;

    @ApiModelProperty(value = "经办人名称")
    private String assigneeName;

    @ApiModelProperty(value = "经办人登录名称")
    private String assigneeLoginName;

    @ApiModelProperty(value = "经办人真实名称")
    private String assigneeRealName;

    @ApiModelProperty(value = "经办人用户标签")
    private List<String> userLabels;

    @ApiModelProperty(value = "报告人名称")
    private String reporterName;

    @ApiModelProperty(value = "报告人登录名称")
    private String reporterLoginName;

    @ApiModelProperty(value = "报告人真实名称")
    private String reporterRealName;

    @ApiModelProperty(value = "报告人图标")
    private String reporterImageUrl;

    @ApiModelProperty(value = "经办人图标")
    private String assigneeImageUrl;

    @ApiModelProperty(value = "史诗名称")
    private String epicName;

    @ApiModelProperty(value = "史诗id")
    @Encrypt(ignoreValue = {"0"})
    private Long epicId;

    @ApiModelProperty(value = "史诗颜色")
    private String epicColor;

    @ApiModelProperty(value = "故事点")
    private BigDecimal storyPoints;

    @ApiModelProperty(value = "特性名称")
    private String featureName;

    @ApiModelProperty(value = "特性id")
    @Encrypt(ignoreValue = {"0"})
    private Long featureId;

    @ApiModelProperty(value = "特性颜色")
    private String featureColor;

    @ApiModelProperty(value = "如果问题类型是特性，返回特性类别:business、enabler")
    private String featureType;

    @ApiModelProperty(value = "是否添加问题")
    private Boolean addIssue;

    @ApiModelProperty(value = "剩余时间")
    private BigDecimal remainingTime;

    @ApiModelProperty(value = "优先级DTO")
    private PriorityVO priorityVO;

    @ApiModelProperty(value = "状态DTO")
    private StatusVO statusMapVO;

    @ApiModelProperty(value = "问题类型DTO")
    private IssueTypeVO issueTypeVO;

    @ApiModelProperty(value = "创建时间")
    private Date creationDate;

    @ApiModelProperty(value = "最后更新时间")
    private Date lastUpdateDate;

    @Encrypt(ignoreValue = {"0"})
    private Long parentId;

    @ApiModelProperty(value = "关联的版本")
    private List<VersionIssueRelVO> versionIssueRelVOS;

    @ApiModelProperty(value = "关联的标签")
    private List<LabelIssueRelVO> labelIssueRelVOS;

    @ApiModelProperty(value = "冲刺列表")
    private List<IssueSprintVO> issueSprintVOS;

    @ApiModelProperty(value = "评论列表")
    private List<IssueComponentBriefVO> issueComponentBriefVOS;

    @ApiModelProperty(value = "自定义字段kv")
    private Map<String, Object> foundationFieldValue;

    @ApiModelProperty("项目信息")
    private ProjectVO projectVO;

    private Date estimatedStartTime;

    private Date estimatedEndTime;

    private Date actualStartTime;

    private Date actualEndTime;

    @ApiModelProperty("项目名")
    private String projectName;

    @ApiModelProperty("特性关联团队")
    private List<ProjectVO> featureTeams;

    @ApiModelProperty("星标")
    private Boolean starBeacon;

    @Encrypt
    private Long mainResponsibleId;

    private String environment;

    @ApiModelProperty("主要负责人")
    private UserMessageDTO mainResponsibleUser;

    @ApiModelProperty("环境")
    private String environmentName;

    @ApiModelProperty(value = "修复的版本")
    private List<VersionIssueRelVO> fixVersionIssueRelVOS;

    @ApiModelProperty(value = "影响的版本")
    private List<VersionIssueRelVO> influenceVersionIssueRelVOS;

    private UserMessageDTO createUser;

    private UserMessageDTO updateUser;

    @ApiModelProperty("已耗费时间")
    private BigDecimal spentWorkTime;

    @ApiModelProperty("总预估时间")
    private BigDecimal allEstimateTime;

    @ApiModelProperty("tags")
    private List<TagVO> tags;

    private List<ProjectVO> projectVOList;

    private String epicSelfName;

    @ApiModelProperty("参与人")
    private List<UserMessageDTO> participants;

    @ApiModelProperty("预估时间")
    private BigDecimal estimateTime;

    @ApiModelProperty("工时")
    private BigDecimal workTime;

    @ApiModelProperty("历史累计工时")
    private BigDecimal cumulativeWorkTime;

    @ApiModelProperty("偏差率")
    private BigDecimal deviationRate;

    @ApiModelProperty(value = "瀑布工作项的完成进度")
    private Integer progress;

    public Integer getProgress() {
        return progress;
    }

    public void setProgress(Integer progress) {
        this.progress = progress;
    }

    public String getEpicSelfName() {
        return epicSelfName;
    }

    public void setEpicSelfName(String epicSelfName) {
        this.epicSelfName = epicSelfName;
    }

    public List<ProjectVO> getProjectVOList() {
        return projectVOList;
    }

    public void setProjectVOList(List<ProjectVO> projectVOList) {
        this.projectVOList = projectVOList;
    }

    public List<TagVO> getTags() {
        return tags;
    }

    public void setTags(List<TagVO> tags) {
        this.tags = tags;
    }

    public Boolean getStarBeacon() {
        return starBeacon;
    }

    public void setStarBeacon(Boolean starBeacon) {
        this.starBeacon = starBeacon;
    }

    public List<ProjectVO> getFeatureTeams() {
        return featureTeams;
    }

    public void setFeatureTeams(List<ProjectVO> featureTeams) {
        this.featureTeams = featureTeams;
    }

    public String getProjectName() {
        return projectName;
    }

    public void setProjectName(String projectName) {
        this.projectName = projectName;
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

    public Long getParentId() {
        return parentId;
    }

    public void setParentId(Long parentId) {
        this.parentId = parentId;
    }

    public List<IssueSprintVO> getIssueSprintVOS() {
        return issueSprintVOS;
    }

    public List<LabelIssueRelVO> getLabelIssueRelVOS() {
        return labelIssueRelVOS;
    }

    public void setLabelIssueRelVOS(List<LabelIssueRelVO> labelIssueRelVOS) {
        this.labelIssueRelVOS = labelIssueRelVOS;
    }

    public void setIssueSprintVOS(List<IssueSprintVO> issueSprintVOS) {
        this.issueSprintVOS = issueSprintVOS;
    }

    public List<IssueComponentBriefVO> getIssueComponentBriefVOS() {
        return issueComponentBriefVOS;
    }

    public void setIssueComponentBriefVOS(List<IssueComponentBriefVO> issueComponentBriefVOS) {
        this.issueComponentBriefVOS = issueComponentBriefVOS;
    }

    public Long getEpicId() {
        return epicId;
    }

    public void setEpicId(Long epicId) {
        this.epicId = epicId;
    }

    public Long getIssueTypeId() {
        return issueTypeId;
    }

    public void setIssueTypeId(Long issueTypeId) {
        this.issueTypeId = issueTypeId;

    }

    public String getEpicColor() {
        return epicColor;
    }

    public void setEpicColor(String epicColor) {
        this.epicColor = epicColor;
    }

    public Date getCreationDate() {
        return creationDate;
    }

    public void setCreationDate(Date creationDate) {
        this.creationDate = creationDate;
    }

    public Date getLastUpdateDate() {
        return lastUpdateDate;
    }

    public void setLastUpdateDate(Date lastUpdateDate) {
        this.lastUpdateDate = lastUpdateDate;
    }

    public Long getReporterId() {
        return reporterId;
    }

    public void setReporterId(Long reporterId) {
        this.reporterId = reporterId;
    }

    public String getReporterName() {
        return reporterName;
    }

    public void setReporterName(String reporterName) {
        this.reporterName = reporterName;
    }

    public List<VersionIssueRelVO> getVersionIssueRelVOS() {
        return versionIssueRelVOS;
    }

    public void setVersionIssueRelVOS(List<VersionIssueRelVO> versionIssueRelVOS) {
        this.versionIssueRelVOS = versionIssueRelVOS;
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

    public String getTypeCode() {
        return typeCode;
    }

    public void setTypeCode(String typeCode) {
        this.typeCode = typeCode;
    }

    public String getSummary() {
        return summary;
    }

    public void setSummary(String summary) {
        this.summary = summary;
    }

    public Long getAssigneeId() {
        return assigneeId;
    }

    public void setAssigneeId(Long assigneeId) {
        this.assigneeId = assigneeId;
    }

    public Long getProjectId() {
        return projectId;
    }

    public void setProjectId(Long projectId) {
        this.projectId = projectId;
    }

    public String getAssigneeName() {
        return assigneeName;
    }

    public void setAssigneeName(String assigneeName) {
        this.assigneeName = assigneeName;
    }

    public String getReporterImageUrl() {
        return reporterImageUrl;
    }

    public void setReporterImageUrl(String reporterImageUrl) {
        this.reporterImageUrl = reporterImageUrl;
    }

    public String getAssigneeImageUrl() {
        return assigneeImageUrl;
    }

    public void setAssigneeImageUrl(String assigneeImageUrl) {
        this.assigneeImageUrl = assigneeImageUrl;
    }

    public void setStoryPoints(BigDecimal storyPoints) {
        this.storyPoints = storyPoints;
    }

    public BigDecimal getStoryPoints() {
        return storyPoints;
    }

    public Boolean getAddIssue() {
        return addIssue;
    }

    public void setAddIssue(Boolean addIssue) {
        this.addIssue = addIssue;
    }

    public String getEpicName() {
        return epicName;
    }

    public void setEpicName(String epicName) {
        this.epicName = epicName;
    }

    public BigDecimal getRemainingTime() {
        return remainingTime;
    }

    public void setRemainingTime(BigDecimal remainingTime) {
        this.remainingTime = remainingTime;
    }

    public void setPriorityVO(PriorityVO priorityVO) {
        this.priorityVO = priorityVO;
    }

    public PriorityVO getPriorityVO() {
        return priorityVO;
    }

    public void setStatusVO(StatusVO statusMapVO) {
        this.statusMapVO = statusMapVO;
    }

    public StatusVO getStatusVO() {
        return statusMapVO;
    }

    public void setIssueTypeVO(IssueTypeVO issueTypeVO) {
        this.issueTypeVO = issueTypeVO;
    }

    public IssueTypeVO getIssueTypeVO() {
        return issueTypeVO;
    }

    public String getAssigneeLoginName() {
        return assigneeLoginName;
    }

    public void setAssigneeLoginName(String assigneeLoginName) {
        this.assigneeLoginName = assigneeLoginName;
    }

    public String getAssigneeRealName() {
        return assigneeRealName;
    }

    public void setAssigneeRealName(String assigneeRealName) {
        this.assigneeRealName = assigneeRealName;
    }

    public String getReporterLoginName() {
        return reporterLoginName;
    }

    public void setReporterLoginName(String reporterLoginName) {
        this.reporterLoginName = reporterLoginName;
    }

    public String getReporterRealName() {
        return reporterRealName;
    }

    public void setReporterRealName(String reporterRealName) {
        this.reporterRealName = reporterRealName;
    }

    public void setFeatureType(String featureType) {
        this.featureType = featureType;
    }

    public String getFeatureType() {
        return featureType;
    }

    public Map<String, Object> getFoundationFieldValue() {
        return foundationFieldValue;
    }

    public void setFoundationFieldValue(Map<String, Object> foundationFieldValue) {
        this.foundationFieldValue = foundationFieldValue;
    }

    public String getFeatureName() {
        return featureName;
    }

    public void setFeatureName(String featureName) {
        this.featureName = featureName;
    }

    public Long getFeatureId() {
        return featureId;
    }

    public void setFeatureId(Long featureId) {
        this.featureId = featureId;
    }

    public String getFeatureColor() {
        return featureColor;
    }

    public void setFeatureColor(String featureColor) {
        this.featureColor = featureColor;
    }

    @Override
    public String toString() {
        return StringUtil.getToString(this);
    }

    public ProjectVO getProjectVO() {
        return projectVO;
    }

    public void setProjectVO(ProjectVO projectVO) {
        this.projectVO = projectVO;
    }

    public Long getMainResponsibleId() {
        return mainResponsibleId;
    }

    public void setMainResponsibleId(Long mainResponsibleId) {
        this.mainResponsibleId = mainResponsibleId;
    }

    public String getEnvironment() {
        return environment;
    }

    public void setEnvironment(String environment) {
        this.environment = environment;
    }

    public List<VersionIssueRelVO> getFixVersionIssueRelVOS() {
        return fixVersionIssueRelVOS;
    }

    public void setFixVersionIssueRelVOS(List<VersionIssueRelVO> fixVersionIssueRelVOS) {
        this.fixVersionIssueRelVOS = fixVersionIssueRelVOS;
    }

    public List<VersionIssueRelVO> getInfluenceVersionIssueRelVOS() {
        return influenceVersionIssueRelVOS;
    }

    public void setInfluenceVersionIssueRelVOS(List<VersionIssueRelVO> influenceVersionIssueRelVOS) {
        this.influenceVersionIssueRelVOS = influenceVersionIssueRelVOS;
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

    public List<UserMessageDTO> getParticipants() {
        return participants;
    }

    public void setParticipants(List<UserMessageDTO> participants) {
        this.participants = participants;
    }

    public BigDecimal getEstimateTime() {
        return estimateTime;
    }

    public void setEstimateTime(BigDecimal estimateTime) {
        this.estimateTime = estimateTime;
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

    public BigDecimal getDeviationRate() {
        return deviationRate;
    }

    public void setDeviationRate(BigDecimal deviationRate) {
        this.deviationRate = deviationRate;
    }

    public List<String> getUserLabels() {
        return userLabels;
    }

    public void setUserLabels(List<String> userLabels) {
        this.userLabels = userLabels;
    }

    public StatusVO getStatusMapVO() {
        return statusMapVO;
    }

    public void setStatusMapVO(StatusVO statusMapVO) {
        this.statusMapVO = statusMapVO;
    }
}
