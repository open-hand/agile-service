package io.choerodon.agile.infra.dto.business;

import java.math.BigDecimal;
import java.util.Date;
import java.util.List;
import java.util.Set;

import com.fasterxml.jackson.annotation.JsonInclude;

import io.choerodon.agile.api.vo.IssueTypeVO;
import io.choerodon.agile.api.vo.PriorityVO;
import io.choerodon.agile.api.vo.StatusVO;
import io.choerodon.agile.api.vo.business.TagVO;
import io.choerodon.agile.infra.dto.*;
import io.choerodon.agile.infra.utils.StringUtil;

/**
 * @author dinghuang123@gmail.com
 */
@JsonInclude(JsonInclude.Include.NON_NULL)
public class IssueDetailDTO {

    private Long issueId;

    private String issueNum;

    private String typeCode;

    private Long statusId;

    private String summary;

    private Long reporterId;

    private String description;

    private Long assigneeId;

    private Long projectId;

    private Long epicId;

    private Long parentIssueId;

    private BigDecimal storyPoints;

    private Long objectVersionNumber;

    private Long priorityId;

    private Long issueTypeId;

    private List<VersionIssueRelDTO> versionIssueRelDTOList;

    private List<LabelIssueRelDTO> labelIssueRelDTOList;

    private List<ComponentIssueRelDTO> componentIssueRelDTOList;

    private List<IssueLinkDTO> issueLinkDTOList;

    private SprintNameDTO activeSprint;

    private List<SprintNameDTO> closeSprint;

    private PiNameDTO activePi;

    private List<PiNameDTO> closePi;

    private List<IssueCommentDTO> issueCommentDTOList;

    private List<IssueAttachmentDTO> issueAttachmentDTOList;

    private List<IssueDTO> subIssueDTOList;

    private List<IssueDTO> subBugDOList;

    private List<IssueDTO> sameParentIssueDTOList;

    private List<IssueDTO> sameParentBugDOList;

    private Date creationDate;

    private Date lastUpdateDate;

    private BigDecimal estimateTime;

    private BigDecimal remainingTime;

    private String epicName;

    private String issueEpicName;

    private String color;

    private String epicColor;

    private String rank;

    private String parentIssueNum;

    private String parentIssueSummary;

    private String parentIssueDescription;

    private PriorityVO priorityVO;

    private IssueTypeVO issueTypeVO;

    private StatusVO statusVO;

    private Long createdBy;

    private String applyType;

    private String issueTypeCode;

    private FeatureDTO featureDTO;

    private Long featureId;

    private String featureName;

    private Long relateIssueId;

    private String relateIssueNum;

    private String parentRelateSummary;

    private WsjfDTO wsjf;

    private Set<Long> projectIds;

    private Date estimatedStartTime;

    private Date estimatedEndTime;

    private String parentRelateDescription;

    private Long mainResponsibleId;

    private String environment;

    private Long lastUpdatedBy;

    private List<TagVO> tags;

    private String priorityCode;

    private Date actualStartTime;

    private Date actualEndTime;

    private List<Long> participantIds;

    private List<Long> productIds;

    public String getPriorityCode() {
        return priorityCode;
    }

    public IssueDetailDTO setPriorityCode(String priorityCode) {
        this.priorityCode = priorityCode;
        return this;
    }

    public List<TagVO> getTags() {
        return tags;
    }

    public IssueDetailDTO setTags(List<TagVO> tags) {
        this.tags = tags;
        return this;
    }

    public String getParentRelateDescription() {
        return parentRelateDescription;
    }

    public IssueDetailDTO setParentRelateDescription(String parentRelateDescription) {
        this.parentRelateDescription = parentRelateDescription;
        return this;
    }

    public String getParentIssueDescription() {
        return parentIssueDescription;
    }

    public IssueDetailDTO setParentIssueDescription(String parentIssueDescription) {
        this.parentIssueDescription = parentIssueDescription;
        return this;
    }

    public List<IssueDTO> getSameParentIssueDTOList() {
        return sameParentIssueDTOList;
    }

    public IssueDetailDTO setSameParentIssueDTOList(List<IssueDTO> sameParentIssueDTOList) {
        this.sameParentIssueDTOList = sameParentIssueDTOList;
        return this;
    }

    public List<IssueDTO> getSameParentBugDOList() {
        return sameParentBugDOList;
    }

    public IssueDetailDTO setSameParentBugDOList(List<IssueDTO> sameParentBugDOList) {
        this.sameParentBugDOList = sameParentBugDOList;
        return this;
    }

    public Date getEstimatedStartTime() {
        return estimatedStartTime;
    }

    public IssueDetailDTO setEstimatedStartTime(Date estimatedStartTime) {
        this.estimatedStartTime = estimatedStartTime;
        return this;
    }

    public Date getEstimatedEndTime() {
        return estimatedEndTime;
    }

    public IssueDetailDTO setEstimatedEndTime(Date estimatedEndTime) {
        this.estimatedEndTime = estimatedEndTime;
        return this;
    }

    public String getIssueTypeCode() {
        return issueTypeCode;
    }

    public IssueDetailDTO setIssueTypeCode(String issueTypeCode) {
        this.issueTypeCode = issueTypeCode;
        return this;
    }

    public StatusVO getStatusVO() {
        return statusVO;
    }

    public IssueDetailDTO setStatusVO(StatusVO statusVO) {
        this.statusVO = statusVO;
        return this;
    }

    public Long getIssueId() {
        return issueId;
    }

    public IssueDetailDTO setIssueId(Long issueId) {
        this.issueId = issueId;
        return this;
    }

    public String getIssueNum() {
        return issueNum;
    }

    public IssueDetailDTO setIssueNum(String issueNum) {
        this.issueNum = issueNum;
        return this;
    }

    public String getTypeCode() {
        return typeCode;
    }

    public IssueDetailDTO setTypeCode(String typeCode) {
        this.typeCode = typeCode;
        return this;
    }

    public Long getStatusId() {
        return statusId;
    }

    public IssueDetailDTO setStatusId(Long statusId) {
        this.statusId = statusId;
        return this;
    }

    public String getSummary() {
        return summary;
    }

    public IssueDetailDTO setSummary(String summary) {
        this.summary = summary;
        return this;
    }

    public Long getReporterId() {
        return reporterId;
    }

    public IssueDetailDTO setReporterId(Long reporterId) {
        this.reporterId = reporterId;
        return this;
    }

    public String getDescription() {
        return description;
    }

    public IssueDetailDTO setDescription(String description) {
        this.description = description;
        return this;
    }

    public Long getAssigneeId() {
        return assigneeId;
    }

    public IssueDetailDTO setAssigneeId(Long assigneeId) {
        this.assigneeId = assigneeId;
        return this;
    }

    public Long getProjectId() {
        return projectId;
    }

    public IssueDetailDTO setProjectId(Long projectId) {
        this.projectId = projectId;
        return this;
    }

    public Long getEpicId() {
        return epicId;
    }

    public IssueDetailDTO setEpicId(Long epicId) {
        this.epicId = epicId;
        return this;
    }

    public Long getParentIssueId() {
        return parentIssueId;
    }

    public IssueDetailDTO setParentIssueId(Long parentIssueId) {
        this.parentIssueId = parentIssueId;
        return this;
    }

    public IssueDetailDTO setStoryPoints(BigDecimal storyPoints) {
        this.storyPoints = storyPoints;
        return this;
    }

    public BigDecimal getStoryPoints() {
        return storyPoints;
    }

    public Long getObjectVersionNumber() {
        return objectVersionNumber;
    }

    public IssueDetailDTO setObjectVersionNumber(Long objectVersionNumber) {
        this.objectVersionNumber = objectVersionNumber;
        return this;
    }

    public List<VersionIssueRelDTO> getVersionIssueRelDTOList() {
        return versionIssueRelDTOList;
    }

    public IssueDetailDTO setVersionIssueRelDTOList(List<VersionIssueRelDTO> versionIssueRelDTOList) {
        this.versionIssueRelDTOList = versionIssueRelDTOList;
        return this;
    }

    public List<LabelIssueRelDTO> getLabelIssueRelDTOList() {
        return labelIssueRelDTOList;
    }

    public IssueDetailDTO setLabelIssueRelDTOList(List<LabelIssueRelDTO> labelIssueRelDTOList) {
        this.labelIssueRelDTOList = labelIssueRelDTOList;
        return this;
    }

    public List<ComponentIssueRelDTO> getComponentIssueRelDTOList() {
        return componentIssueRelDTOList;
    }

    public IssueDetailDTO setComponentIssueRelDTOList(List<ComponentIssueRelDTO> componentIssueRelDTOList) {
        this.componentIssueRelDTOList = componentIssueRelDTOList;
        return this;
    }

    public List<IssueLinkDTO> getIssueLinkDTOList() {
        return issueLinkDTOList;
    }

    public IssueDetailDTO setIssueLinkDTOList(List<IssueLinkDTO> issueLinkDTOList) {
        this.issueLinkDTOList = issueLinkDTOList;
        return this;
    }

    public List<IssueCommentDTO> getIssueCommentDTOList() {
        return issueCommentDTOList;
    }

    public IssueDetailDTO setIssueCommentDTOList(List<IssueCommentDTO> issueCommentDTOList) {
        this.issueCommentDTOList = issueCommentDTOList;
        return this;
    }

    public List<IssueAttachmentDTO> getIssueAttachmentDTOList() {
        return issueAttachmentDTOList;
    }

    public IssueDetailDTO setIssueAttachmentDTOList(List<IssueAttachmentDTO> issueAttachmentDTOList) {
        this.issueAttachmentDTOList = issueAttachmentDTOList;
        return this;
    }

    public List<IssueDTO> getSubIssueDTOList() {
        return subIssueDTOList;
    }

    public IssueDetailDTO setSubIssueDTOList(List<IssueDTO> subIssueDTOList) {
        this.subIssueDTOList = subIssueDTOList;
        return this;
    }

    public Date getCreationDate() {
        return creationDate;
    }

    public IssueDetailDTO setCreationDate(Date creationDate) {
        this.creationDate = creationDate;
        return this;
    }

    public Date getLastUpdateDate() {
        return lastUpdateDate;
    }

    public IssueDetailDTO setLastUpdateDate(Date lastUpdateDate) {
        this.lastUpdateDate = lastUpdateDate;
        return this;
    }

    public BigDecimal getEstimateTime() {
        return estimateTime;
    }

    public IssueDetailDTO setEstimateTime(BigDecimal estimateTime) {
        this.estimateTime = estimateTime;
        return this;
    }

    public BigDecimal getRemainingTime() {
        return remainingTime;
    }

    public IssueDetailDTO setRemainingTime(BigDecimal remainingTime) {
        this.remainingTime = remainingTime;
        return this;
    }

    public String getEpicName() {
        return epicName;
    }

    public IssueDetailDTO setEpicName(String epicName) {
        this.epicName = epicName;
        return this;
    }

    public IssueDetailDTO setIssueEpicName(String issueEpicName) {
        this.issueEpicName = issueEpicName;
        return this;
    }

    public String getIssueEpicName() {
        return issueEpicName;
    }

    public String getColor() {
        return color;
    }

    public IssueDetailDTO setColor(String color) {
        this.color = color;
        return this;
    }

    public String getEpicColor() {
        return epicColor;
    }

    public IssueDetailDTO setEpicColor(String epicColor) {
        this.epicColor = epicColor;
        return this;
    }

    public String getRank() {
        return rank;
    }

    public IssueDetailDTO setRank(String rank) {
        this.rank = rank;
        return this;
    }

    public String getParentIssueNum() {
        return parentIssueNum;
    }

    public IssueDetailDTO setParentIssueNum(String parentIssueNum) {
        this.parentIssueNum = parentIssueNum;
        return this;
    }

    public IssueDetailDTO setParentIssueSummary(String parentIssueSummary) {
        this.parentIssueSummary = parentIssueSummary;
        return this;
    }

    public String getParentIssueSummary() {
        return parentIssueSummary;
    }

    public SprintNameDTO getActiveSprint() {
        return activeSprint;
    }

    public IssueDetailDTO setActiveSprint(SprintNameDTO activeSprint) {
        this.activeSprint = activeSprint;
        return this;
    }

    public List<SprintNameDTO> getCloseSprint() {
        return closeSprint;
    }

    public IssueDetailDTO setCloseSprint(List<SprintNameDTO> closeSprint) {
        this.closeSprint = closeSprint;
        return this;
    }

    public IssueDetailDTO setPriorityId(Long priorityId) {
        this.priorityId = priorityId;
        return this;
    }

    public Long getPriorityId() {
        return priorityId;
    }

    public IssueDetailDTO setPriorityVO(PriorityVO priorityVO) {
        this.priorityVO = priorityVO;
        return this;
    }

    public PriorityVO getPriorityVO() {
        return priorityVO;
    }

    public IssueDetailDTO setIssueTypeId(Long issueTypeId) {
        this.issueTypeId = issueTypeId;
        return this;
    }

    public Long getIssueTypeId() {
        return issueTypeId;
    }

    public IssueDetailDTO setIssueTypeVO(IssueTypeVO issueTypeVO) {
        this.issueTypeVO = issueTypeVO;
        return this;
    }

    public IssueTypeVO getIssueTypeVO() {
        return issueTypeVO;
    }

    public IssueDetailDTO setCreatedBy(Long createdBy) {
        this.createdBy = createdBy;
        return this;
    }

    public Long getCreatedBy() {
        return createdBy;
    }

    public String getApplyType() {
        return applyType;
    }

    public IssueDetailDTO setApplyType(String applyType) {
        this.applyType = applyType;
        return this;
    }

    public IssueDetailDTO setFeatureDTO(FeatureDTO featureDTO) {
        this.featureDTO = featureDTO;
        return this;
    }

    public FeatureDTO getFeatureDTO() {
        return featureDTO;
    }

    public Long getFeatureId() {
        return featureId;
    }

    public IssueDetailDTO setFeatureId(Long featureId) {
        this.featureId = featureId;
        return this;
    }

    public String getFeatureName() {
        return featureName;
    }

    public IssueDetailDTO setFeatureName(String featureName) {
        this.featureName = featureName;
        return this;
    }

    public IssueDetailDTO setActivePi(PiNameDTO activePi) {
        this.activePi = activePi;
        return this;
    }

    public PiNameDTO getActivePi() {
        return activePi;
    }

    public IssueDetailDTO setClosePi(List<PiNameDTO> closePi) {
        this.closePi = closePi;
        return this;
    }

    public List<PiNameDTO> getClosePi() {
        return closePi;
    }

    public IssueDetailDTO setRelateIssueId(Long relateIssueId) {
        this.relateIssueId = relateIssueId;
        return this;
    }

    public Long getRelateIssueId() {
        return relateIssueId;
    }

    public IssueDetailDTO setSubBugDOList(List<IssueDTO> subBugDOList) {
        this.subBugDOList = subBugDOList;
        return this;
    }

    public List<IssueDTO> getSubBugDOList() {
        return subBugDOList;
    }

    public IssueDetailDTO setRelateIssueNum(String relateIssueNum) {
        this.relateIssueNum = relateIssueNum;
        return this;
    }

    public String getRelateIssueNum() {
        return relateIssueNum;
    }

    public IssueDetailDTO setParentRelateSummary(String parentRelateSummary) {
        this.parentRelateSummary = parentRelateSummary;
        return this;
    }

    public String getParentRelateSummary() {
        return parentRelateSummary;
    }

    @Override
    public String toString() {
        return StringUtil.getToString(this);
    }

    public WsjfDTO getWsjf() {
        return wsjf;
    }

    public IssueDetailDTO setWsjf(WsjfDTO wsjf) {
        this.wsjf = wsjf;
        return this;
    }

    public Set<Long> getProjectIds() {
        return projectIds;
    }

    public IssueDetailDTO setProjectIds(Set<Long> projectIds) {
        this.projectIds = projectIds;
        return this;
    }

    public Long getMainResponsibleId() {
        return mainResponsibleId;
    }

    public IssueDetailDTO setMainResponsibleId(Long mainResponsibleId) {
        this.mainResponsibleId = mainResponsibleId;
        return this;
    }

    public String getEnvironment() {
        return environment;
    }

    public IssueDetailDTO setEnvironment(String environment) {
        this.environment = environment;
        return this;
    }

    public Long getLastUpdatedBy() {
        return lastUpdatedBy;
    }

    public IssueDetailDTO setLastUpdatedBy(Long lastUpdatedBy) {
        this.lastUpdatedBy = lastUpdatedBy;
        return this;
    }

    public Date getActualStartTime() {
        return actualStartTime;
    }

    public IssueDetailDTO setActualStartTime(Date actualStartTime) {
        this.actualStartTime = actualStartTime;
        return this;
    }

    public Date getActualEndTime() {
        return actualEndTime;
    }

    public IssueDetailDTO setActualEndTime(Date actualEndTime) {
        this.actualEndTime = actualEndTime;
        return this;
    }

    public List<Long> getParticipantIds() {
        return participantIds;
    }

    public IssueDetailDTO setParticipantIds(List<Long> participantIds) {
        this.participantIds = participantIds;
        return this;
    }

    public List<Long> getProductIds() {
        return productIds;
    }

    public IssueDetailDTO setProductIds(List<Long> productIds) {
        this.productIds = productIds;
        return this;
    }
}
