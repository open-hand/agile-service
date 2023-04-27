package io.choerodon.agile.api.vo.business;

import java.math.BigDecimal;
import java.util.List;

import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.infra.dto.UserMessageDTO;
import io.choerodon.agile.infra.utils.StringUtil;

/**
 * Created by WangZhe@choerodon.io on 2019-06-28.
 * Email: ettwz@hotmail.com
 */
public class IssueListFieldKVVO extends BaseIssueVO {

    @ApiModelProperty(value = "经办人id")
    private Long assigneeId;

    @ApiModelProperty(value = "项目id")
    private Long projectId;

    @ApiModelProperty(value = "经办人名称")
    private String assigneeName;

    @ApiModelProperty(value = "经办人登录名称")
    private String assigneeLoginName;

    @ApiModelProperty(value = "经办人真实名称")
    private String assigneeRealName;

    @ApiModelProperty(value = "经办人用户标签")
    private List<String> userLabels;

    @ApiModelProperty(value = "经办人图标")
    private String assigneeImageUrl;

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

    @ApiModelProperty("项目信息")
    private ProjectVO projectVO;

    @ApiModelProperty("项目名")
    private String projectName;

    @ApiModelProperty("特性关联团队")
    private List<ProjectVO> featureTeams;

    @ApiModelProperty("星标")
    private Boolean starBeacon;

    @Encrypt
    @ApiModelProperty(value = "主要负责人id")
    private Long mainResponsibleId;
    @ApiModelProperty(value = "环境")
    private String environment;

    @ApiModelProperty("主要负责人")
    private UserMessageDTO mainResponsibleUser;

    @ApiModelProperty("环境")
    private String environmentName;

    @ApiModelProperty(value = "修复的版本")
    private List<VersionIssueRelVO> fixVersionIssueRelVOS;

    @ApiModelProperty(value = "影响的版本")
    private List<VersionIssueRelVO> influenceVersionIssueRelVOS;

    @ApiModelProperty("已耗费时间")
    private BigDecimal spentWorkTime;

    @ApiModelProperty("总预估时间")
    private BigDecimal allEstimateTime;

    @ApiModelProperty(value = "项目集")
    private List<ProjectVO> projectVOList;

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

    @ApiModelProperty(value = "产品")
    private List<ProductVO> productVOList;

    @ApiModelProperty(value = "敏捷子任务数量")
    private Integer totalSubIssues;

    @ApiModelProperty(value = "敏捷已完成子任务数量")
    private Integer completedSubIssues;
    @ApiModelProperty(value = "第三方实例关联关系")
    private List<InstanceOpenRelVO> instanceOpenRelList;

    public List<InstanceOpenRelVO> getInstanceOpenRelList() {
        return instanceOpenRelList;
    }

    public void setInstanceOpenRelList(List<InstanceOpenRelVO> instanceOpenRelList) {
        this.instanceOpenRelList = instanceOpenRelList;
    }

    public Integer getProgress() {
        return progress;
    }

    public void setProgress(Integer progress) {
        this.progress = progress;
    }

    public List<ProjectVO> getProjectVOList() {
        return projectVOList;
    }

    public void setProjectVOList(List<ProjectVO> projectVOList) {
        this.projectVOList = projectVOList;
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

    public List<VersionIssueRelVO> getVersionIssueRelVOS() {
        return versionIssueRelVOS;
    }

    public void setVersionIssueRelVOS(List<VersionIssueRelVO> versionIssueRelVOS) {
        this.versionIssueRelVOS = versionIssueRelVOS;
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

    public String getAssigneeImageUrl() {
        return assigneeImageUrl;
    }

    public void setAssigneeImageUrl(String assigneeImageUrl) {
        this.assigneeImageUrl = assigneeImageUrl;
    }

    public Boolean getAddIssue() {
        return addIssue;
    }

    public void setAddIssue(Boolean addIssue) {
        this.addIssue = addIssue;
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

    public void setFeatureType(String featureType) {
        this.featureType = featureType;
    }

    public String getFeatureType() {
        return featureType;
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

    public List<ProductVO> getProductVOList() {
        return productVOList;
    }

    public void setProductVOList(List<ProductVO> productVOList) {
        this.productVOList = productVOList;
    }

    public Integer getTotalSubIssues() {
        return totalSubIssues;
    }

    public void setTotalSubIssues(Integer totalSubIssues) {
        this.totalSubIssues = totalSubIssues;
    }

    public Integer getCompletedSubIssues() {
        return completedSubIssues;
    }

    public void setCompletedSubIssues(Integer completedSubIssues) {
        this.completedSubIssues = completedSubIssues;
    }

}
