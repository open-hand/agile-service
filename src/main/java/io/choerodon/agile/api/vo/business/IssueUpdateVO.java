package io.choerodon.agile.api.vo.business;


import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.api.vo.waterfall.WaterfallIssueVO;
import io.choerodon.agile.infra.annotation.Update;
import io.choerodon.agile.infra.utils.StringUtil;
import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

import java.math.BigDecimal;
import java.util.Date;
import java.util.List;

/**
 * @author dinghuang123@gmail.com
 */
public class IssueUpdateVO {

    @ApiModelProperty(value = "问题主键id")
    @Encrypt
    private Long issueId;

    @ApiModelProperty(value = "问题编号")
    private String issueNum;

    @ApiModelProperty(value = "状态id")
    @Encrypt
    private Long statusId;

    @ApiModelProperty(value = "问题概要")
    private String summary;

    @ApiModelProperty(value = "优先级code")
    @Encrypt
    private String priorityCode;

    @ApiModelProperty(value = "报告人id")
    @Encrypt
    private Long reporterId;

    @ApiModelProperty(value = "描述")
    private String description;

    @ApiModelProperty(value = "经办人id")
    @Encrypt
    private Long assigneeId;

    @ApiModelProperty(value = "史诗id")
    @Encrypt(ignoreValue = {"0"})
    private Long epicId;

    @ApiModelProperty(value = "冲刺id")
    @Encrypt(ignoreValue = {"0"})
    private Long sprintId;

    @ApiModelProperty(value = "父任务id")
    @Encrypt(ignoreValue = {"0"})
    private Long parentIssueId;

    @ApiModelProperty(value = "故事点")
    private BigDecimal storyPoints;

    @ApiModelProperty(value = "问题排序字段")
    private String rank;

    @ApiModelProperty(value = "版本类型：fix、influence")
    @Update(temp = true)
    private String versionType;

    @ApiModelProperty(value = "关联的版本列表")
    private List<VersionIssueRelVO> versionIssueRelVOList;

    @ApiModelProperty(value = "关联的标签列表")
    private List<LabelIssueRelVO> labelIssueRelVOList;

    @ApiModelProperty(value = "关联的模块列表")
    private List<ComponentIssueRelVO> componentIssueRelVOList;

    @ApiModelProperty(value = "关联的问题链接列表")
    private List<IssueLinkVO> issueLinkVOList;

    @ApiModelProperty(value = "关联的附件列表")
    private List<IssueAttachmentVO> issueAttachmentVOList;

    @ApiModelProperty(value = "版本号")
    private Long objectVersionNumber;

    @ApiModelProperty(value = "预估时间")
    private BigDecimal estimateTime;

    @ApiModelProperty(value = "剩余时间")
    private BigDecimal remainingTime;

    @ApiModelProperty(value = "史诗颜色")
    private String colorCode;

    @ApiModelProperty(value = "史诗名称")
    private String epicName;

    @ApiModelProperty(value = "优先级id")
    @Encrypt(ignoreValue = {"0"})
    private Long priorityId;

    @ApiModelProperty(value = "停留时间")
    private Date stayDate;

    @ApiModelProperty(value = "关联的featureDTO")
    private FeatureVO featureVO;

    @ApiModelProperty(value = "关联的featureId")
    @Encrypt(ignoreValue = {"0"})
    private Long featureId;

    @ApiModelProperty(value = "bug关联的故事id")
    @Encrypt(ignoreValue = {"0"})
    private Long relateIssueId;

    private Date estimatedStartTime;

    private Date estimatedEndTime;

    @ApiModelProperty(value = "状态是否自动流转更新")
    private Boolean autoTranferFlag;

    @ApiModelProperty(value = "自动触发issueId")
    private Long autoTriggerId;

    @ApiModelProperty(value = "自动触发issueNum")
    private String autoTriggerNum;

    @Encrypt
    private Long mainResponsibleId;

    private String environment;

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

    @ApiModelProperty(value = "风险工作项")
    private RiskVO riskVO;

    public List<TagVO> getTags() {
        return tags;
    }

    public void setTags(List<TagVO> tags) {
        this.tags = tags;
    }

    public Boolean getAutoTranferFlag() {
        return autoTranferFlag;
    }

    public void setAutoTranferFlag(Boolean autoTranferFlag) {
        this.autoTranferFlag = autoTranferFlag;
    }

    public Long getAutoTriggerId() {
        return autoTriggerId;
    }

    public void setAutoTriggerId(Long autoTriggerId) {
        this.autoTriggerId = autoTriggerId;
    }

    public String getAutoTriggerNum() {
        return autoTriggerNum;
    }

    public void setAutoTriggerNum(String autoTriggerNum) {
        this.autoTriggerNum = autoTriggerNum;
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

    public String getRank() {
        return rank;
    }

    public void setRank(String rank) {
        this.rank = rank;
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

    public Long getStatusId() {
        return statusId;
    }

    public void setStatusId(Long statusId) {
        this.statusId = statusId;
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

    public Long getReporterId() {
        return reporterId;
    }

    public void setReporterId(Long reporterId) {
        this.reporterId = reporterId;
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

    public Long getParentIssueId() {
        return parentIssueId;
    }

    public void setParentIssueId(Long parentIssueId) {
        this.parentIssueId = parentIssueId;
    }

    public void setStoryPoints(BigDecimal storyPoints) {
        this.storyPoints = storyPoints;
    }

    public BigDecimal getStoryPoints() {
        return storyPoints;
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

    public List<IssueLinkVO> getIssueLinkVOList() {
        return issueLinkVOList;
    }

    public void setIssueLinkVOList(List<IssueLinkVO> issueLinkVOList) {
        this.issueLinkVOList = issueLinkVOList;
    }

    public List<IssueAttachmentVO> getIssueAttachmentVOList() {
        return issueAttachmentVOList;
    }

    public void setIssueAttachmentVOList(List<IssueAttachmentVO> issueAttachmentVOList) {
        this.issueAttachmentVOList = issueAttachmentVOList;
    }

    public Long getObjectVersionNumber() {
        return objectVersionNumber;
    }

    public void setObjectVersionNumber(Long objectVersionNumber) {
        this.objectVersionNumber = objectVersionNumber;
    }

    public BigDecimal getEstimateTime() {
        return estimateTime;
    }

    public void setEstimateTime(BigDecimal estimateTime) {
        this.estimateTime = estimateTime;
    }

    public String getColorCode() {
        return colorCode;
    }

    public void setColorCode(String colorCode) {
        this.colorCode = colorCode;
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

    public void setVersionType(String versionType) {
        this.versionType = versionType;
    }

    public String getVersionType() {
        return versionType;
    }

    public void setPriorityId(Long priorityId) {
        this.priorityId = priorityId;
    }

    public Long getPriorityId() {
        return priorityId;
    }

    public void setStayDate(Date stayDate) {
        this.stayDate = stayDate;
    }

    public Date getStayDate() {
        return stayDate;
    }

    public FeatureVO getFeatureVO() {
        return featureVO;
    }

    public void setFeatureVO(FeatureVO featureVO) {
        this.featureVO = featureVO;
    }

    public Long getFeatureId() {
        return featureId;
    }

    public void setFeatureId(Long featureId) {
        this.featureId = featureId;
    }

    public void setRelateIssueId(Long relateIssueId) {
        this.relateIssueId = relateIssueId;
    }

    public Long getRelateIssueId() {
        return relateIssueId;
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

    @Override
    public String toString() {
        return StringUtil.getToString(this);
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

    public RiskVO getRiskVO() {
        return riskVO;
    }

    public void setRiskVO(RiskVO riskVO) {
        this.riskVO = riskVO;
    }
}
