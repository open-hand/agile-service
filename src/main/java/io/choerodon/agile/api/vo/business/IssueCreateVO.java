package io.choerodon.agile.api.vo.business;


import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.api.vo.waterfall.WaterfallIssueVO;
import io.choerodon.agile.api.vo.waterfall.WfDeliverableVO;
import io.choerodon.agile.infra.utils.StringUtil;
import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

import java.math.BigDecimal;
import java.util.Date;
import java.util.List;

/**
 * @author dinghuang123@gmail.com
 */
public class IssueCreateVO {

    @ApiModelProperty(value = "问题类型code")
    private String typeCode;

    @ApiModelProperty(value = "问题概要")
    private String summary;

    @ApiModelProperty(value = "优先级code")
    private String priorityCode;

    @ApiModelProperty(value = "描述")
    private String description;

    @ApiModelProperty(value = "经办人id")
    @Encrypt
    private Long assigneeId;

    @ApiModelProperty(value = "报告人id")
    @Encrypt
    private Long reporterId;

    @ApiModelProperty(value = "项目id")
    private Long projectId;

    @ApiModelProperty(value = "史诗id")
    @Encrypt(ignoreValue = {"0"})
    private Long epicId;

    @ApiModelProperty(value = "冲刺id")
    @Encrypt(ignoreValue = {"0"})
    private Long sprintId;

    @ApiModelProperty(value = "优先级id")
    @Encrypt(ignoreValue = {"0"})
    private Long priorityId;

    @ApiModelProperty(value = "问题类型id")
    @Encrypt
    private Long issueTypeId;

    @ApiModelProperty(value = "关联的版本列表")
    private List<VersionIssueRelVO> versionIssueRelVOList;

    @ApiModelProperty(value = "关联的标签列表")
    private List<LabelIssueRelVO> labelIssueRelVOList;

    @ApiModelProperty(value = "关联的问题链接列表")
    private List<IssueLinkCreateVO> issueLinkCreateVOList;

    @ApiModelProperty(value = "关联的模块列表")
    private List<ComponentIssueRelVO> componentIssueRelVOList;

    @ApiModelProperty(value = "剩余时间")
    private BigDecimal remainingTime;

    @ApiModelProperty(value = "预估时间")
    private BigDecimal estimateTime;

    @ApiModelProperty(value = "史诗名称")
    private String epicName;

    @ApiModelProperty(value = "故事点")
    private BigDecimal storyPoints;

    @Encrypt(ignoreValue = {"0"})
    private Long relateIssueId;

    @Encrypt(ignoreValue = {"0"})
    private Long parentIssueId;

    private RankVO rankVO;

    @ApiModelProperty(value = "featureId")
    @Encrypt(ignoreValue = {"0"})
    private Long featureId;

    @ApiModelProperty(value = "feature开始时间")
    private Date startDate;

    @ApiModelProperty(value = "feature结束时间")
    private Date endDate;

    @ApiModelProperty(value = "feature所属项目群id")
    private Long programId;

    @ApiModelProperty(value = "问题所属pi")
    @Encrypt(ignoreValue = {"0"})
    private Long piId;

    @ApiModelProperty(value = "featureVO")
    private FeatureVO featureVO;

    @ApiModelProperty(value = "交付物")
    private List<WfDeliverableVO> wfDeliverableVOS;

    private WsjfVO wsjfVO;

    private Long teamProjectId;

    private Boolean isEpic;

    private List<Long> teamProjectIds;

    private Date estimatedStartTime;

    private Date estimatedEndTime;

    private List<PageFieldViewUpdateVO> customFields;

    private RelatedIssueVO relatedIssueVO;

    @Encrypt
    private Long mainResponsibleId;

    private String environment;

    @Encrypt
    private List<Long> programVersionIds;

    @Encrypt
    private Long statusId;

    private List<TagVO> tags;

    @ApiModelProperty(value = "实际开始时间")
    private Date actualStartTime;

    @ApiModelProperty(value = "实际结束时间")
    private Date actualEndTime;

    @ApiModelProperty(value = "参与人")
    @Encrypt
    private List<Long> participantIds;

    @ApiModelProperty(value = "瀑布工作项")
    private WaterfallIssueVO waterfallIssueVO;

    public List<TagVO> getTags() {
        return tags;
    }

    public void setTags(List<TagVO> tags) {
        this.tags = tags;
    }

    public List<Long> getProgramVersionIds() {
        return programVersionIds;
    }

    public void setProgramVersionIds(List<Long> programVersionIds) {
        this.programVersionIds = programVersionIds;
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

    public RelatedIssueVO getRelatedIssueVO() {
        return relatedIssueVO;
    }

    public void setRelatedIssueVO(RelatedIssueVO relatedIssueVO) {
        this.relatedIssueVO = relatedIssueVO;
    }

    public List<PageFieldViewUpdateVO> getCustomFields() {
        return customFields;
    }

    public void setCustomFields(List<PageFieldViewUpdateVO> customFields) {
        this.customFields = customFields;
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

    public Boolean getEpic() {
        return isEpic;
    }

    public void setEpic(Boolean epic) {
        isEpic = epic;
    }

    public Long getTeamProjectId() {
        return teamProjectId;
    }

    public void setTeamProjectId(Long teamProjectId) {
        this.teamProjectId = teamProjectId;
    }

    public List<IssueLinkCreateVO> getIssueLinkCreateVOList() {
        return issueLinkCreateVOList;
    }

    public void setIssueLinkCreateVOList(List<IssueLinkCreateVO> issueLinkCreateVOList) {
        this.issueLinkCreateVOList = issueLinkCreateVOList;
    }

    public void setStoryPoints(BigDecimal storyPoints) {
        this.storyPoints = storyPoints;
    }

    public BigDecimal getStoryPoints() {
        return storyPoints;
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

    public String getPriorityCode() {
        return priorityCode;
    }

    public void setPriorityCode(String priorityCode) {
        this.priorityCode = priorityCode;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
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

    public Long getEpicId() {
        return epicId;
    }

    public void setEpicId(Long epicId) {
        this.epicId = epicId;
    }

    public Long getSprintId() {
        return sprintId;
    }

    public void setSprintId(Long sprintId) {
        this.sprintId = sprintId;
    }

    public List<VersionIssueRelVO> getVersionIssueRelVOList() {
        return versionIssueRelVOList;
    }

    public void setVersionIssueRelVOList(List<VersionIssueRelVO> versionIssueRelVOList) {
        this.versionIssueRelVOList = versionIssueRelVOList;
    }

    public List<LabelIssueRelVO> getLabelIssueRelVOList() {
        return labelIssueRelVOList;
    }

    public void setLabelIssueRelVOList(List<LabelIssueRelVO> labelIssueRelVOList) {
        this.labelIssueRelVOList = labelIssueRelVOList;
    }

    public List<ComponentIssueRelVO> getComponentIssueRelVOList() {
        return componentIssueRelVOList;
    }

    public void setComponentIssueRelVOList(List<ComponentIssueRelVO> componentIssueRelVOList) {
        this.componentIssueRelVOList = componentIssueRelVOList;
    }

    public BigDecimal getRemainingTime() {
        return remainingTime;
    }

    public BigDecimal getEstimateTime() {
        return estimateTime;
    }

    public void setEstimateTime(BigDecimal estimateTime) {
        this.estimateTime = estimateTime;
    }

    public void setRemainingTime(BigDecimal remainingTime) {
        this.remainingTime = remainingTime;
    }

    public String getEpicName() {
        return epicName;
    }

    public void setEpicName(String epicName) {
        this.epicName = epicName;
    }

    public Long getReporterId() {
        return reporterId;
    }

    public void setReporterId(Long reporterId) {
        this.reporterId = reporterId;
    }

    public void setPriorityId(Long priorityId) {
        this.priorityId = priorityId;
    }

    public Long getPriorityId() {
        return priorityId;
    }

    public void setIssueTypeId(Long issueTypeId) {
        this.issueTypeId = issueTypeId;
    }

    public Long getIssueTypeId() {
        return issueTypeId;
    }

    public void setRelateIssueId(Long relateIssueId) {
        this.relateIssueId = relateIssueId;
    }

    public Long getRelateIssueId() {
        return relateIssueId;
    }

    public void setRankVO(RankVO rankVO) {
        this.rankVO = rankVO;
    }

    public RankVO getRankVO() {
        return rankVO;
    }

    public Long getFeatureId() {
        return featureId;
    }

    public void setFeatureId(Long featureId) {
        this.featureId = featureId;
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

    public Long getProgramId() {
        return programId;
    }

    public void setProgramId(Long programId) {
        this.programId = programId;
    }

    public Long getPiId() {
        return piId;
    }

    public void setPiId(Long piId) {
        this.piId = piId;
    }

    public FeatureVO getFeatureVO() {
        return featureVO;
    }

    public void setFeatureVO(FeatureVO featureVO) {
        this.featureVO = featureVO;
    }


    public Long getParentIssueId() {
        return parentIssueId;
    }

    public void setParentIssueId(Long parentIssueId) {
        this.parentIssueId = parentIssueId;
    }

    @Override
    public String toString() {
        return StringUtil.getToString(this);
    }

    public WsjfVO getWsjfVO() {
        return wsjfVO;
    }

    public void setWsjfVO(WsjfVO wsjfVO) {
        this.wsjfVO = wsjfVO;
    }

    public List<Long> getTeamProjectIds() {
        return teamProjectIds;
    }

    public void setTeamProjectIds(List<Long> teamProjectIds) {
        this.teamProjectIds = teamProjectIds;
    }

    public Long getStatusId() {
        return statusId;
    }

    public void setStatusId(Long statusId) {
        this.statusId = statusId;
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

    public List<Long> getParticipantIds() {
        return participantIds;
    }

    public void setParticipantIds(List<Long> participantIds) {
        this.participantIds = participantIds;
    }

    public WaterfallIssueVO getWaterfallIssueVO() {
        return waterfallIssueVO;
    }

    public void setWaterfallIssueVO(WaterfallIssueVO waterfallIssueVO) {
        this.waterfallIssueVO = waterfallIssueVO;
    }

    public List<WfDeliverableVO> getWfDeliverableVOS() {
        return wfDeliverableVOS;
    }

    public void setWfDeliverableVOS(List<WfDeliverableVO> wfDeliverableVOS) {
        this.wfDeliverableVOS = wfDeliverableVOS;
    }
}
