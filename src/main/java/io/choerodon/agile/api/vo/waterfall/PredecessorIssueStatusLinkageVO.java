package io.choerodon.agile.api.vo.waterfall;

import io.choerodon.agile.api.vo.IssueTypeVO;
import io.choerodon.agile.api.vo.StatusVO;
import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

/**
 * @author huaxin.deng@hand-china.com
 * @since 2022/4/1
 */
public class PredecessorIssueStatusLinkageVO {
    @Encrypt
    @ApiModelProperty(value = "主键")
    private Long id;

    @Encrypt
    @ApiModelProperty(value = "问题类型id")
    private Long issueTypeId;

    @Encrypt
    @ApiModelProperty(value = "状态id")
    private Long statusId;
    @ApiModelProperty(value = "前置项类型")
    private String predecessorType;

    @Encrypt
    @ApiModelProperty(value = "前置项问题类型id")
    private Long predecessorIssueTypeId;
    @ApiModelProperty(value = "前置项问题类型")
    private IssueTypeVO predecessorIssueType;

    @Encrypt
    @ApiModelProperty(value = "前置项问题状态id")
    private Long predecessorIssueStatusId;
    @ApiModelProperty(value = "前置项问题状态")
    private StatusVO predecessorIssueStatus;
    @ApiModelProperty(value = "状态")
    private StatusVO statusVO;
    @ApiModelProperty(value = "问题类型")
    private IssueTypeVO issueTypeVO;
    @ApiModelProperty(value = "项目id")
    private Long projectId;
    @ApiModelProperty(value = "组织id")
    private Long organizationId;
    @ApiModelProperty(value = "是否被触发")
    private Boolean isTriggered;

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public Long getIssueTypeId() {
        return issueTypeId;
    }

    public void setIssueTypeId(Long issueTypeId) {
        this.issueTypeId = issueTypeId;
    }

    public Long getStatusId() {
        return statusId;
    }

    public void setStatusId(Long statusId) {
        this.statusId = statusId;
    }

    public String getPredecessorType() {
        return predecessorType;
    }

    public void setPredecessorType(String predecessorType) {
        this.predecessorType = predecessorType;
    }

    public Long getPredecessorIssueTypeId() {
        return predecessorIssueTypeId;
    }

    public void setPredecessorIssueTypeId(Long predecessorIssueTypeId) {
        this.predecessorIssueTypeId = predecessorIssueTypeId;
    }

    public IssueTypeVO getPredecessorIssueType() {
        return predecessorIssueType;
    }

    public void setPredecessorIssueType(IssueTypeVO predecessorIssueType) {
        this.predecessorIssueType = predecessorIssueType;
    }

    public Long getPredecessorIssueStatusId() {
        return predecessorIssueStatusId;
    }

    public void setPredecessorIssueStatusId(Long predecessorIssueStatusId) {
        this.predecessorIssueStatusId = predecessorIssueStatusId;
    }

    public StatusVO getPredecessorIssueStatus() {
        return predecessorIssueStatus;
    }

    public void setPredecessorIssueStatus(StatusVO predecessorIssueStatus) {
        this.predecessorIssueStatus = predecessorIssueStatus;
    }

    public StatusVO getStatusVO() {
        return statusVO;
    }

    public void setStatusVO(StatusVO statusVO) {
        this.statusVO = statusVO;
    }

    public IssueTypeVO getIssueTypeVO() {
        return issueTypeVO;
    }

    public void setIssueTypeVO(IssueTypeVO issueTypeVO) {
        this.issueTypeVO = issueTypeVO;
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

    public Boolean getTriggered() {
        return isTriggered;
    }

    public void setTriggered(Boolean triggered) {
        isTriggered = triggered;
    }
}
