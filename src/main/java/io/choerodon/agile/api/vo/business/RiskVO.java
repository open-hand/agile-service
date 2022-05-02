package io.choerodon.agile.api.vo.business;

import io.choerodon.agile.api.vo.IssueSubListVO;
import io.choerodon.agile.infra.dto.UserMessageDTO;
import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

import java.util.Date;
import java.util.List;

/**
 * @author huaxin.deng@hand-china.com
 * @since 2022/4/12
 */
public class RiskVO {

    @ApiModelProperty(value = "主键id")
    @Encrypt
    private Long id;

    @ApiModelProperty(value = "问题id")
    @Encrypt
    private Long issueId;

    @ApiModelProperty(value = "风险分类id")
    @Encrypt
    private Long categoryId;

    @ApiModelProperty(value = "风险分类")
    private RiskCategoryVO categoryVO;

    @ApiModelProperty(value = "影响度id")
    @Encrypt
    private Long influenceId;

    @ApiModelProperty(value = "影响度")
    private RiskInfluenceVO influenceVO;

    @ApiModelProperty(value = "发生概率id")
    @Encrypt
    private Long probabilityId;

    @ApiModelProperty(value = "发生概率")
    private RiskProbabilityVO probabilityVO;

    @ApiModelProperty(value = "临近度id")
    @Encrypt
    private Long proximityId;

    @ApiModelProperty(value = "临近度")
    private RiskProximityVO proximityVO;

    @ApiModelProperty(value = "应对策略")
    private String copingStrategy;

    @ApiModelProperty(value = "预计解决日期")
    private Date estimatedResolutionDate;

    @ApiModelProperty(value = "实际解决日期")
    private Date actualResolutionDate;

    @ApiModelProperty(value = "发现日期")
    private Date discoveryDate;

    @ApiModelProperty(value = "相关方人员ids")
    @Encrypt
    private List<Long> relatedPartyIds;

    @ApiModelProperty(value = "相关方")
    private List<UserMessageDTO> relatedParties;

    @ApiModelProperty(value = "来源工作项ids")
    @Encrypt
    private List<Long> sourceIssueIds;

    @ApiModelProperty(value = "来源工作项")
    private List<IssueSubListVO> sourceIssueList;

    @ApiModelProperty(value = "执行工作项ids")
    @Encrypt
    private List<Long> executeIssueIds;

    @ApiModelProperty(value = "来源工作项")
    private List<IssueSubListVO> executeIssueList;

    @ApiModelProperty(value = "项目id")
    private Long projectId;

    @ApiModelProperty(value = "组织id")
    private Long organizationId;

    @ApiModelProperty(value = "关联的风险id")
    @Encrypt
    private Long riskRelId;

    @ApiModelProperty(value = "关联类型")
    private String issueRelType;

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public Long getIssueId() {
        return issueId;
    }

    public void setIssueId(Long issueId) {
        this.issueId = issueId;
    }

    public Long getCategoryId() {
        return categoryId;
    }

    public void setCategoryId(Long categoryId) {
        this.categoryId = categoryId;
    }

    public RiskCategoryVO getCategoryVO() {
        return categoryVO;
    }

    public void setCategoryVO(RiskCategoryVO categoryVO) {
        this.categoryVO = categoryVO;
    }

    public Long getInfluenceId() {
        return influenceId;
    }

    public void setInfluenceId(Long influenceId) {
        this.influenceId = influenceId;
    }

    public RiskInfluenceVO getInfluenceVO() {
        return influenceVO;
    }

    public void setInfluenceVO(RiskInfluenceVO influenceVO) {
        this.influenceVO = influenceVO;
    }

    public Long getProbabilityId() {
        return probabilityId;
    }

    public void setProbabilityId(Long probabilityId) {
        this.probabilityId = probabilityId;
    }

    public RiskProbabilityVO getProbabilityVO() {
        return probabilityVO;
    }

    public void setProbabilityVO(RiskProbabilityVO probabilityVO) {
        this.probabilityVO = probabilityVO;
    }

    public Long getProximityId() {
        return proximityId;
    }

    public void setProximityId(Long proximityId) {
        this.proximityId = proximityId;
    }

    public RiskProximityVO getProximityVO() {
        return proximityVO;
    }

    public void setProximityVO(RiskProximityVO proximityVO) {
        this.proximityVO = proximityVO;
    }

    public String getCopingStrategy() {
        return copingStrategy;
    }

    public void setCopingStrategy(String copingStrategy) {
        this.copingStrategy = copingStrategy;
    }

    public Date getEstimatedResolutionDate() {
        return estimatedResolutionDate;
    }

    public void setEstimatedResolutionDate(Date estimatedResolutionDate) {
        this.estimatedResolutionDate = estimatedResolutionDate;
    }

    public Date getActualResolutionDate() {
        return actualResolutionDate;
    }

    public void setActualResolutionDate(Date actualResolutionDate) {
        this.actualResolutionDate = actualResolutionDate;
    }

    public Date getDiscoveryDate() {
        return discoveryDate;
    }

    public void setDiscoveryDate(Date discoveryDate) {
        this.discoveryDate = discoveryDate;
    }

    public List<Long> getRelatedPartyIds() {
        return relatedPartyIds;
    }

    public void setRelatedPartyIds(List<Long> relatedPartyIds) {
        this.relatedPartyIds = relatedPartyIds;
    }

    public List<UserMessageDTO> getRelatedParties() {
        return relatedParties;
    }

    public void setRelatedParties(List<UserMessageDTO> relatedParties) {
        this.relatedParties = relatedParties;
    }

    public List<Long> getSourceIssueIds() {
        return sourceIssueIds;
    }

    public void setSourceIssueIds(List<Long> sourceIssueIds) {
        this.sourceIssueIds = sourceIssueIds;
    }

    public List<IssueSubListVO> getSourceIssueList() {
        return sourceIssueList;
    }

    public void setSourceIssueList(List<IssueSubListVO> sourceIssueList) {
        this.sourceIssueList = sourceIssueList;
    }

    public List<Long> getExecuteIssueIds() {
        return executeIssueIds;
    }

    public void setExecuteIssueIds(List<Long> executeIssueIds) {
        this.executeIssueIds = executeIssueIds;
    }

    public List<IssueSubListVO> getExecuteIssueList() {
        return executeIssueList;
    }

    public void setExecuteIssueList(List<IssueSubListVO> executeIssueList) {
        this.executeIssueList = executeIssueList;
    }

    public Long getProjectId() {
        return projectId;
    }

    public void setProjectId(Long projectId) {
        this.projectId = projectId;
    }

    public Long getOrganizationId() {
        return organizationId;
    }

    public void setOrganizationId(Long organizationId) {
        this.organizationId = organizationId;
    }

    public Long getRiskRelId() {
        return riskRelId;
    }

    public void setRiskRelId(Long riskRelId) {
        this.riskRelId = riskRelId;
    }

    public String getIssueRelType() {
        return issueRelType;
    }

    public void setIssueRelType(String issueRelType) {
        this.issueRelType = issueRelType;
    }
}
