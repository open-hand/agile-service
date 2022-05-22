package io.choerodon.agile.api.vo;

import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

/**
 * @author zhaotianxin
 * @date 2020-08-17 19:13
 */
public class StatusLinkageVO {
    @Encrypt
    @ApiModelProperty(value = "id")
    private Long id;

    @Encrypt
    @ApiModelProperty(value = "问题类型id")
    private Long issueTypeId;

    @Encrypt
    @ApiModelProperty(value = "状态id")
    private Long statusId;
    @ApiModelProperty(value = "项目id")
    private Long projectId;
    @ApiModelProperty(value = "父级问题类型编码")
    private String  parentIssueTypeCode;

    @Encrypt
    @ApiModelProperty(value = "父级问题状态联动配置")
    private Long parentIssueStatusSetting;
    @ApiModelProperty(value = "问题类型")
    private IssueTypeVO issueTypeVO;
    @ApiModelProperty(value = "状态")
    private StatusVO statusVO;
    @ApiModelProperty(value = "项目群id")
    private Long programId;
    @ApiModelProperty(value = "项目")
    private ProjectVO projectVO;

    @Encrypt
    @ApiModelProperty(value = "父级问题类型id")
    private Long parentIssueTypeId;
    @ApiModelProperty(value = "问题类型名")
    private String issueTypeName;
    @ApiModelProperty(value = "类型")
    private String type;

    public String getIssueTypeName() {
        return issueTypeName;
    }

    public void setIssueTypeName(String issueTypeName) {
        this.issueTypeName = issueTypeName;
    }

    public Long getParentIssueTypeId() {
        return parentIssueTypeId;
    }

    public void setParentIssueTypeId(Long parentIssueTypeId) {
        this.parentIssueTypeId = parentIssueTypeId;
    }

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

    public Long getProjectId() {
        return projectId;
    }

    public void setProjectId(Long projectId) {
        this.projectId = projectId;
    }

    public String getParentIssueTypeCode() {
        return parentIssueTypeCode;
    }

    public void setParentIssueTypeCode(String parentIssueTypeCode) {
        this.parentIssueTypeCode = parentIssueTypeCode;
    }

    public Long getParentIssueStatusSetting() {
        return parentIssueStatusSetting;
    }

    public void setParentIssueStatusSetting(Long parentIssueStatusSetting) {
        this.parentIssueStatusSetting = parentIssueStatusSetting;
    }

    public IssueTypeVO getIssueTypeVO() {
        return issueTypeVO;
    }

    public void setIssueTypeVO(IssueTypeVO issueTypeVO) {
        this.issueTypeVO = issueTypeVO;
    }

    public StatusVO getStatusVO() {
        return statusVO;
    }

    public void setStatusVO(StatusVO statusVO) {
        this.statusVO = statusVO;
    }

    public Long getProgramId() {
        return programId;
    }

    public void setProgramId(Long programId) {
        this.programId = programId;
    }

    public ProjectVO getProjectVO() {
        return projectVO;
    }

    public void setProjectVO(ProjectVO projectVO) {
        this.projectVO = projectVO;
    }

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }
}
