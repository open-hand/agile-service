package io.choerodon.agile.api.vo;

import java.math.BigDecimal;
import java.util.Date;
import java.util.List;
import java.util.Map;

import io.swagger.annotations.ApiModelProperty;

import io.choerodon.agile.api.vo.business.TagVO;
import io.choerodon.agile.infra.dto.UserMessageDTO;

import org.hzero.starter.keyencrypt.core.Encrypt;

/**
 * @author superlee
 * @since 2023/4/11
 */
public class BaseIssueVO {

    @ApiModelProperty(value = "问题主键id")
    @Encrypt
    private Long issueId;

    @ApiModelProperty(value = "问题编号")
    private String issueNum;

    @ApiModelProperty(value = "问题类型code")
    private String typeCode;

    @ApiModelProperty(value = "问题概要")
    private String summary;

    @ApiModelProperty(value = "问题类型id")
    @Encrypt
    private Long issueTypeId;

    @ApiModelProperty(value = "故事点")
    private BigDecimal storyPoints;

    @ApiModelProperty(value = "史诗id")
    @Encrypt(ignoreValue = {"0"})
    private Long epicId;

    @ApiModelProperty(value = "史诗名称")
    private String epicName;

    @ApiModelProperty(value = "史诗颜色")
    private String epicColor;

    @ApiModelProperty(value = "报告人id")
    private Long reporterId;

    @ApiModelProperty(value = "报告人名称")
    private String reporterName;

    @ApiModelProperty(value = "报告人登录名称")
    private String reporterLoginName;

    @ApiModelProperty(value = "报告人真实名称")
    private String reporterRealName;

    @ApiModelProperty(value = "报告人图标")
    private String reporterImageUrl;

    @ApiModelProperty(value = "创建时间")
    private Date creationDate;

    @ApiModelProperty(value = "最后更新时间")
    private Date lastUpdateDate;

    @ApiModelProperty(value = "问题类型DTO")
    private IssueTypeVO issueTypeVO;

    @ApiModelProperty(value = "预计开始时间")
    private Date estimatedStartTime;

    @ApiModelProperty(value = "预计结束时间")
    private Date estimatedEndTime;
    @ApiModelProperty(value = "实际开始时间")
    private Date actualStartTime;

    @ApiModelProperty(value = "实际结束时间")
    private Date actualEndTime;

    @ApiModelProperty(value = "自定义字段kv")
    private Map<String, Object> foundationFieldValue;

    @ApiModelProperty(value = "创建人")
    private UserMessageDTO createUser;
    @ApiModelProperty(value = "更新人")
    private UserMessageDTO updateUser;

    @ApiModelProperty("tags")
    private List<TagVO> tags;
    @ApiModelProperty(value = "史诗自己的名字")
    private String epicSelfName;
    @ApiModelProperty(value = "关联需求")
    private List<BacklogInfoVO> relateBacklogs;
    @ApiModelProperty(value = "来源需求")
    private List<BacklogInfoVO> sourceBacklogs;

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

    public Long getIssueTypeId() {
        return issueTypeId;
    }

    public void setIssueTypeId(Long issueTypeId) {
        this.issueTypeId = issueTypeId;
    }

    public BigDecimal getStoryPoints() {
        return storyPoints;
    }

    public void setStoryPoints(BigDecimal storyPoints) {
        this.storyPoints = storyPoints;
    }

    public Long getEpicId() {
        return epicId;
    }

    public void setEpicId(Long epicId) {
        this.epicId = epicId;
    }

    public String getEpicName() {
        return epicName;
    }

    public void setEpicName(String epicName) {
        this.epicName = epicName;
    }

    public String getEpicColor() {
        return epicColor;
    }

    public void setEpicColor(String epicColor) {
        this.epicColor = epicColor;
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

    public String getReporterImageUrl() {
        return reporterImageUrl;
    }

    public void setReporterImageUrl(String reporterImageUrl) {
        this.reporterImageUrl = reporterImageUrl;
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

    public IssueTypeVO getIssueTypeVO() {
        return issueTypeVO;
    }

    public void setIssueTypeVO(IssueTypeVO issueTypeVO) {
        this.issueTypeVO = issueTypeVO;
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

    public Map<String, Object> getFoundationFieldValue() {
        return foundationFieldValue;
    }

    public void setFoundationFieldValue(Map<String, Object> foundationFieldValue) {
        this.foundationFieldValue = foundationFieldValue;
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

    public List<TagVO> getTags() {
        return tags;
    }

    public void setTags(List<TagVO> tags) {
        this.tags = tags;
    }

    public String getEpicSelfName() {
        return epicSelfName;
    }

    public void setEpicSelfName(String epicSelfName) {
        this.epicSelfName = epicSelfName;
    }

    public List<BacklogInfoVO> getRelateBacklogs() {
        return relateBacklogs;
    }

    public void setRelateBacklogs(List<BacklogInfoVO> relateBacklogs) {
        this.relateBacklogs = relateBacklogs;
    }

    public List<BacklogInfoVO> getSourceBacklogs() {
        return sourceBacklogs;
    }

    public void setSourceBacklogs(List<BacklogInfoVO> sourceBacklogs) {
        this.sourceBacklogs = sourceBacklogs;
    }
}
