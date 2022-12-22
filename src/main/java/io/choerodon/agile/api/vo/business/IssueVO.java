package io.choerodon.agile.api.vo.business;


import java.math.BigDecimal;
import java.util.Date;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.annotations.ApiModelProperty;

import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.api.vo.waterfall.WaterfallIssueVO;
import io.choerodon.agile.infra.dto.UserMessageDTO;
import io.choerodon.agile.infra.utils.StringUtil;

import org.hzero.starter.keyencrypt.core.Encrypt;

/**
 * 敏捷开发Issue
 *
 * @author dinghuang123@gmail.com
 * @since 2018-05-14 20:30:48
 */
@JsonInclude(JsonInclude.Include.NON_NULL)
public class IssueVO {

    @ApiModelProperty(value = "问题主键id")
    @Encrypt
    private Long issueId;

    @ApiModelProperty(value = "问题编号")
    private String issueNum;

    @ApiModelProperty(value = "问题类型code")
    private String typeCode;

    @ApiModelProperty(value = "状态id")
    @Encrypt
    private Long statusId;

    @ApiModelProperty(value = "问题概要")
    private String summary;

    @ApiModelProperty(value = "报告人id")
    @Encrypt
    private Long reporterId;

    @ApiModelProperty(value = "报告人名称")
    private String reporterName;

    @ApiModelProperty(value = "描述")
    private String description;

    @ApiModelProperty(value = "经办人id")
    @Encrypt
    private Long assigneeId;

    @ApiModelProperty(value = "经办人名称")
    private String assigneeName;

    @ApiModelProperty(value = "项目id")
    private Long projectId;

    @ApiModelProperty(value = "史诗id")
    @Encrypt(ignoreValue = {"0"})
    private Long epicId;

    @ApiModelProperty(value = "父任务id")
    @Encrypt(ignoreValue = {"0"})
    private Long parentIssueId;

    @ApiModelProperty(value = "故事点")
    private BigDecimal storyPoints;

    @ApiModelProperty(value = "关联的版本列表")
    private List<VersionIssueRelVO> versionIssueRelVOList;

    @ApiModelProperty(value = "活跃冲刺")
    private SprintNameVO activeSprint;

    @ApiModelProperty(value = "已关闭的冲刺列表")
    private List<SprintNameVO> closeSprint;

    @ApiModelProperty(value = "关联的标签列表")
    private List<LabelIssueRelVO> labelIssueRelVOList;

    @ApiModelProperty(value = "关联的模块列表")
    private List<ComponentIssueRelVO> componentIssueRelVOList;

    @ApiModelProperty(value = "评论列表")
    private List<IssueCommentVO> issueCommentVOList;

    @ApiModelProperty(value = "附件列表")
    private List<IssueAttachmentVO> issueAttachmentVOList;

    @ApiModelProperty(value = "子任务列表")
    private List<IssueSubListVO> subIssueVOList;

    @ApiModelProperty(value = "子缺陷列表")
    private List<IssueSubListVO> subBugVOList;

    @ApiModelProperty(value = "同父亲子任务列表")
    private List<IssueSubListVO> sameParentIssueVOList;

    @ApiModelProperty(value = "同父亲子缺陷列表")
    private List<IssueSubListVO> sameParentBugVOList;

    @ApiModelProperty(value = "问题版本号")
    private Long objectVersionNumber;

    @ApiModelProperty(value = "创建时间")
    private Date creationDate;

    @ApiModelProperty(value = "最后更新时间")
    private Date lastUpdateDate;

    @ApiModelProperty(value = "预估时间")
    private BigDecimal estimateTime;

    @ApiModelProperty(value = "剩余时间")
    private BigDecimal remainingTime;

    @ApiModelProperty(value = "史诗名称")
    private String epicName;

    @ApiModelProperty(value = "关联史诗名称")
    private String issueEpicName;

    @ApiModelProperty(value = "史诗颜色")
    private String color;

    @ApiModelProperty(value = "史诗颜色")
    private String epicColor;

    @ApiModelProperty(value = "冲刺名称")
    private String sprintName;

    @ApiModelProperty(value = "父任务的问题编码")
    private String parentIssueNum;

    @ApiModelProperty(value = "父任务问题概要")
    private String parentIssueSummary;
    @ApiModelProperty(value = "父级问题描述")
    private String parentIssueDescription;
    @ApiModelProperty(value = "父级关联问题描述")
    private String parentRelateDescription;

    @ApiModelProperty(value = "经办人图标")
    private String assigneeImageUrl;

    @ApiModelProperty(value = "报告人图标")
    private String reporterImageUrl;

    @ApiModelProperty(value = "创建人图标")
    private String createrImageUrl;

    @ApiModelProperty(value = "创建人名称")
    private String createrName;

    @ApiModelProperty(value = "创建人登录名称")
    private String createrLoginName;

    @ApiModelProperty(value = "创建人真实名称")
    private String createrRealName;

    @ApiModelProperty(value = "优先级id")
    @Encrypt(ignoreValue = {"0"})
    private Long priorityId;

    @ApiModelProperty(value = "问题类型id")
    @Encrypt
    private Long issueTypeId;

    @ApiModelProperty(value = "优先级DTO")
    private PriorityVO priorityVO;

    @ApiModelProperty(value = "问题类型DTO")
    private IssueTypeVO issueTypeVO;

    @ApiModelProperty(value = "状态DTO")
    private StatusVO statusMapVO;

    @ApiModelProperty(value = "创建人id")
    @Encrypt
    private Long createdBy;

    @ApiModelProperty(value = "业务类型：agile、test等")
    private String applyType;

    @ApiModelProperty(value = "创建人邮箱")
    private String createrEmail;

    @ApiModelProperty(value = "经办人登录名称")
    private String assigneeLoginName;

    @ApiModelProperty(value = "经办人真实名称")
    private String assigneeRealName;

    @ApiModelProperty(value = "报告人登录名称")
    private String reporterLoginName;

    @ApiModelProperty(value = "报告人真实名称")
    private String reporterRealName;

    @ApiModelProperty(value = "缺陷关联的故事id")
    @Encrypt(ignoreValue = {"0"})
    private Long relateIssueId;

    @ApiModelProperty(value = "缺陷关联的故事编号")
    private String relateIssueNum;

    @ApiModelProperty(value = "子缺陷的父任务概要")
    private String parentRelateSummary;

    @ApiModelProperty(value = "featureVO")
    private FeatureVO featureVO;

    @ApiModelProperty(value = "featureId")
    @Encrypt(ignoreValue = {"0"})
    private Long featureId;

    @ApiModelProperty(value = "feature名称")
    private String featureName;

    @ApiModelProperty(value = "项目群信息")
    private ProjectVO programInfo;

    @ApiModelProperty(value = "活跃pi")
    private PiNameVO activePi;

    @ApiModelProperty(value = "已关闭的pi列表")
    private List<PiNameVO> closePi;
    @ApiModelProperty(value = "活跃的pi团队")
    private List<ProjectVO> activePiTeams;
    @ApiModelProperty(value = "活页的pi冲刺")
    private List<SprintDetailVO>  activePiSprints;
    @ApiModelProperty(value = "关闭的pi冲刺")
    private List<SprintDetailVO>  closedPiSprints;
    @ApiModelProperty(value = "wsjf")
    private WsjfVO wsjf;
    @ApiModelProperty(value = "预计开始时间")
    private Date estimatedStartTime;
    @ApiModelProperty(value = "预计结束时间")
    private Date estimatedEndTime;
    @ApiModelProperty(value = "冲刺id")
    private Long sprintId;
    @ApiModelProperty(value = "pi目标信息")
    private List<PiTargetInfoVO> piTargetInfoVOS;

    @ApiModelProperty(value = "更新人图标")
    private String updaterImageUrl;
    @ApiModelProperty(value = "更新人")
    private UserMessageDTO updater;

    @ApiModelProperty(value = "实际开始时间")
    private Date actualStartTime;

    @ApiModelProperty(value = "实际结束时间")
    private Date actualEndTime;

    @ApiModelProperty(value = "所属项目")
    private ProjectVO projectVO;

    public Long getSprintId() {
        return sprintId;
    }

    public void setSprintId(Long sprintId) {
        this.sprintId = sprintId;
    }
    @ApiModelProperty(value = "是否为星标")
    private Boolean starBeacon;
    @ApiModelProperty(value = "父级是否为星标")
    private Boolean parentStarBeacon;
    @ApiModelProperty(value = "关联为题是否为星标")
    private Boolean relateStarBeacon;
    @ApiModelProperty(value = "项目群版本关联关系")
    private List<ProgramVersionFeatureRelVO> programVersionFeatureRelVOS;
    @ApiModelProperty(value = "错误消息")
    private String errorMsg;
    @ApiModelProperty(value = "主要负责人")
    private UserMessageDTO mainResponsible;

    @ApiModelProperty(value = "参与人")
    private List<UserMessageDTO> participants;
    @ApiModelProperty(value = "环境")
    private String environment;
    @ApiModelProperty(value = "标记")
    private List<TagVO> tags;
    @ApiModelProperty(value = "是否已完成")
    private Boolean completed;
    @ApiModelProperty(value = "实际完成时间")
    private Date actualCompletedDate;

    @Encrypt
    @ApiModelProperty(value = "影响的问题id集合")
    private List<Long> influenceIssueIds;

    @ApiModelProperty(value = "主要负责人id")
    @Encrypt
    private Long mainResponsibleId;
    @ApiModelProperty(value = "瀑布问题对象")
    private WaterfallIssueVO waterfallIssueVO;
    @ApiModelProperty(value = "风险对象")
    private RiskVO riskVO;
    @ApiModelProperty(value = "产品集合")
    private List<ProductVO> productVOList;

    @Encrypt
    @ApiModelProperty(value = "产品id")
    private List<Long> productIds;

    @ApiModelProperty(value = "第三方实例关联")
    private List<InstanceOpenRelVO> instanceOpenRels;

    @ApiModelProperty("工时登记配置")
    private WorkHoursConfigVO workHoursConfigVO;

    public List<Long> getInfluenceIssueIds() {
        return influenceIssueIds;
    }

    public void setInfluenceIssueIds(List<Long> influenceIssueIds) {
        this.influenceIssueIds = influenceIssueIds;
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

    public List<TagVO> getTags() {
        return tags;
    }

    public void setTags(List<TagVO> tags) {
        this.tags = tags;
    }

    public String getErrorMsg() {
        return errorMsg;
    }

    public void setErrorMsg(String errorMsg) {
        this.errorMsg = errorMsg;
    }

    public Boolean getParentStarBeacon() {
        return parentStarBeacon;
    }

    public void setParentStarBeacon(Boolean parentStarBeacon) {
        this.parentStarBeacon = parentStarBeacon;
    }

    public Boolean getRelateStarBeacon() {
        return relateStarBeacon;
    }

    public void setRelateStarBeacon(Boolean relateStarBeacon) {
        this.relateStarBeacon = relateStarBeacon;
    }

    public Boolean getStarBeacon() {
        return starBeacon;
    }

    public void setStarBeacon(Boolean starBeacon) {
        this.starBeacon = starBeacon;
    }

    public String getParentRelateDescription() {
        return parentRelateDescription;
    }

    public void setParentRelateDescription(String parentRelateDescription) {
        this.parentRelateDescription = parentRelateDescription;
    }

    public String getParentIssueDescription() {
        return parentIssueDescription;
    }

    public void setParentIssueDescription(String parentIssueDescription) {
        this.parentIssueDescription = parentIssueDescription;
    }

    public List<IssueSubListVO> getSameParentIssueVOList() {
        return sameParentIssueVOList;
    }

    public void setSameParentIssueVOList(List<IssueSubListVO> sameParentIssueVOList) {
        this.sameParentIssueVOList = sameParentIssueVOList;
    }

    public List<IssueSubListVO> getSameParentBugVOList() {
        return sameParentBugVOList;
    }

    public void setSameParentBugVOList(List<IssueSubListVO> sameParentBugVOList) {
        this.sameParentBugVOList = sameParentBugVOList;
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

    public String getCreaterEmail() {
        return createrEmail;
    }

    public void setCreaterEmail(String createrEmail) {
        this.createrEmail = createrEmail;
    }

    public String getCreaterImageUrl() {
        return createrImageUrl;
    }

    public void setCreaterImageUrl(String createrImageUrl) {
        this.createrImageUrl = createrImageUrl;
    }

    public String getCreaterName() {
        return createrName;
    }

    public void setCreaterName(String createrName) {
        this.createrName = createrName;
    }

    public String getApplyType() {
        return applyType;
    }

    public void setApplyType(String applyType) {
        this.applyType = applyType;
    }

    public Long getIssueTypeId() {
        return issueTypeId;
    }

    public void setIssueTypeId(Long issueTypeId) {
        this.issueTypeId = issueTypeId;
    }

    public StatusVO getStatusVO() {
        return statusMapVO;
    }

    public void setStatusVO(StatusVO statusMapVO) {
        this.statusMapVO = statusMapVO;
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

    public Long getObjectVersionNumber() {
        return objectVersionNumber;
    }

    public void setObjectVersionNumber(Long objectVersionNumber) {
        this.objectVersionNumber = objectVersionNumber;
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

    public List<IssueCommentVO> getIssueCommentVOList() {
        return issueCommentVOList;
    }

    public void setIssueCommentVOList(List<IssueCommentVO> issueCommentVOList) {
        this.issueCommentVOList = issueCommentVOList;
    }

    public List<IssueAttachmentVO> getIssueAttachmentVOList() {
        return issueAttachmentVOList;
    }

    public void setIssueAttachmentVOList(List<IssueAttachmentVO> issueAttachmentVOList) {
        this.issueAttachmentVOList = issueAttachmentVOList;
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

    public String getReporterName() {
        return reporterName;
    }

    public void setReporterName(String reporterName) {
        this.reporterName = reporterName;
    }

    public String getAssigneeName() {
        return assigneeName;
    }

    public void setAssigneeName(String assigneeName) {
        this.assigneeName = assigneeName;
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

    public String getEpicName() {
        return epicName;
    }

    public void setEpicName(String epicName) {
        this.epicName = epicName;
    }

    public void setIssueEpicName(String issueEpicName) {
        this.issueEpicName = issueEpicName;
    }

    public String getIssueEpicName() {
        return issueEpicName;
    }

    public String getColor() {
        return color;
    }

    public void setColor(String color) {
        this.color = color;
    }

    public String getEpicColor() {
        return epicColor;
    }

    public void setEpicColor(String epicColor) {
        this.epicColor = epicColor;
    }

    public String getSprintName() {
        return sprintName;
    }

    public void setSprintName(String sprintName) {
        this.sprintName = sprintName;
    }

    public String getParentIssueNum() {
        return parentIssueNum;
    }

    public void setParentIssueNum(String parentIssueNum) {
        this.parentIssueNum = parentIssueNum;
    }

    public void setParentIssueSummary(String parentIssueSummary) {
        this.parentIssueSummary = parentIssueSummary;
    }

    public String getParentIssueSummary() {
        return parentIssueSummary;
    }

    public SprintNameVO getActiveSprint() {
        return activeSprint;
    }

    public void setActiveSprint(SprintNameVO activeSprint) {
        this.activeSprint = activeSprint;
    }

    public List<SprintNameVO> getCloseSprint() {
        return closeSprint;
    }

    public void setCloseSprint(List<SprintNameVO> closeSprint) {
        this.closeSprint = closeSprint;
    }

    public String getAssigneeImageUrl() {
        return assigneeImageUrl;
    }

    public void setAssigneeImageUrl(String assigneeImageUrl) {
        this.assigneeImageUrl = assigneeImageUrl;
    }

    public String getReporterImageUrl() {
        return reporterImageUrl;
    }

    public void setReporterImageUrl(String reporterImageUrl) {
        this.reporterImageUrl = reporterImageUrl;
    }

    public void setPriorityVO(PriorityVO priorityVO) {
        this.priorityVO = priorityVO;
    }

    public PriorityVO getPriorityVO() {
        return priorityVO;
    }

    public void setPriorityId(Long priorityId) {
        this.priorityId = priorityId;
    }

    public Long getPriorityId() {
        return priorityId;
    }

    public void setIssueTypeVO(IssueTypeVO issueTypeVO) {
        this.issueTypeVO = issueTypeVO;
    }

    public IssueTypeVO getIssueTypeVO() {
        return issueTypeVO;
    }

    public void setCreatedBy(Long createdBy) {
        this.createdBy = createdBy;
    }

    public Long getCreatedBy() {
        return createdBy;
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

    public void setRelateIssueId(Long relateIssueId) {
        this.relateIssueId = relateIssueId;
    }

    public Long getRelateIssueId() {
        return relateIssueId;
    }

    public void setRelateIssueNum(String relateIssueNum) {
        this.relateIssueNum = relateIssueNum;
    }

    public String getRelateIssueNum() {
        return relateIssueNum;
    }

    public String getParentRelateSummary() {
        return parentRelateSummary;
    }

    public void setParentRelateSummary(String parentRelateSummary) {
        this.parentRelateSummary = parentRelateSummary;
    }

    public void setSubIssueVOList(List<IssueSubListVO> subIssueVOList) {
        this.subIssueVOList = subIssueVOList;
    }

    public List<IssueSubListVO> getSubIssueVOList() {
        return subIssueVOList;
    }

    public void setSubBugVOList(List<IssueSubListVO> subBugVOList) {
        this.subBugVOList = subBugVOList;
    }

    public List<IssueSubListVO> getSubBugVOList() {
        return subBugVOList;
    }

    public String getCreaterLoginName() {
        return createrLoginName;
    }

    public void setCreaterLoginName(String createrLoginName) {
        this.createrLoginName = createrLoginName;
    }

    public String getCreaterRealName() {
        return createrRealName;
    }

    public void setCreaterRealName(String createrRealName) {
        this.createrRealName = createrRealName;
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

    public String getFeatureName() {
        return featureName;
    }

    public void setFeatureName(String featureName) {
        this.featureName = featureName;
    }

    /**
     * @return 项目群信息
     */
    public ProjectVO getProgramInfo() {
        return programInfo;
    }

    public void setProgramInfo(ProjectVO programInfo) {
        this.programInfo = programInfo;
    }

    public PiNameVO getActivePi() {
        return activePi;
    }

    public void setActivePi(PiNameVO activePi) {
        this.activePi = activePi;
    }

    public List<PiNameVO> getClosePi() {
        return closePi;
    }

    public void setClosePi(List<PiNameVO> closePi) {
        this.closePi = closePi;
    }

    @Override
    public String toString() {
        return StringUtil.getToString(this);
    }

    public List<ProjectVO> getActivePiTeams() {
        return activePiTeams;
    }

    public void setActivePiTeams(List<ProjectVO> activePiTeams) {
        this.activePiTeams = activePiTeams;
    }

    public List<SprintDetailVO> getActivePiSprints() {
        return activePiSprints;
    }

    public void setActivePiSprints(List<SprintDetailVO> activePiSprints) {
        this.activePiSprints = activePiSprints;
    }

    public List<SprintDetailVO> getClosedPiSprints() {
        return closedPiSprints;
    }

    public void setClosedPiSprints(List<SprintDetailVO> closedPiSprints) {
        this.closedPiSprints = closedPiSprints;
    }

    public WsjfVO getWsjf() {
        return wsjf;
    }

    public void setWsjf(WsjfVO wsjf) {
        this.wsjf = wsjf;
    }

    public List<ProgramVersionFeatureRelVO> getProgramVersionFeatureRelVOS() {
        return programVersionFeatureRelVOS;
    }

    public void setProgramVersionFeatureRelVOS(List<ProgramVersionFeatureRelVO> programVersionFeatureRelVOS) {
        this.programVersionFeatureRelVOS = programVersionFeatureRelVOS;
    }

    public List<PiTargetInfoVO> getPiTargetInfoVOS() {
        return piTargetInfoVOS;
    }

    public void setPiTargetInfoVOS(List<PiTargetInfoVO> piTargetInfoVOS) {
        this.piTargetInfoVOS = piTargetInfoVOS;
    }

    public UserMessageDTO getMainResponsible() {
        return mainResponsible;
    }

    public void setMainResponsible(UserMessageDTO mainResponsible) {
        this.mainResponsible = mainResponsible;
    }

    public String getEnvironment() {
        return environment;
    }

    public void setEnvironment(String environment) {
        this.environment = environment;
    }

    public String getUpdaterImageUrl() {
        return updaterImageUrl;
    }

    public void setUpdaterImageUrl(String updaterImageUrl) {
        this.updaterImageUrl = updaterImageUrl;
    }

    public UserMessageDTO getUpdater() {
        return updater;
    }

    public void setUpdater(UserMessageDTO updater) {
        this.updater = updater;
    }

    public Long getMainResponsibleId() {
        return mainResponsibleId;
    }

    public void setMainResponsibleId(Long mainResponsibleId) {
        this.mainResponsibleId = mainResponsibleId;
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

    public ProjectVO getProjectVO() {
        return projectVO;
    }

    public void setProjectVO(ProjectVO projectVO) {
        this.projectVO = projectVO;
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

    public List<ProductVO> getProductVOList() {
        return productVOList;
    }

    public void setProductVOList(List<ProductVO> productVOList) {
        this.productVOList = productVOList;
    }

    public List<Long> getProductIds() {
        return productIds;
    }

    public void setProductIds(List<Long> productIds) {
        this.productIds = productIds;
    }

    public List<InstanceOpenRelVO> getInstanceOpenRels() {
        return instanceOpenRels;
    }

    public void setInstanceOpenRels(List<InstanceOpenRelVO> instanceOpenRels) {
        this.instanceOpenRels = instanceOpenRels;
    }

    public WorkHoursConfigVO getWorkHoursConfigVO() {
        return workHoursConfigVO;
    }

    public void setWorkHoursConfigVO(WorkHoursConfigVO workHoursConfigVO) {
        this.workHoursConfigVO = workHoursConfigVO;
    }
}
