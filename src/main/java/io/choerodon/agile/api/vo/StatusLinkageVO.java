package io.choerodon.agile.api.vo;

import org.hzero.starter.keyencrypt.core.Encrypt;

/**
 * @author zhaotianxin
 * @date 2020-08-17 19:13
 */
public class StatusLinkageVO {
    @Encrypt
    private Long id;

    @Encrypt
    private Long issueTypeId;

    @Encrypt
    private Long statusId;

    private Long projectId;

    private String  parentIssueTypeCode;

    @Encrypt
    private Long parentIssueStatusSetting;

    private IssueTypeVO issueTypeVO;

    private StatusVO statusVO;

    private Long programId;

    private ProjectVO projectVO;

    @Encrypt
    private Long parentIssueTypeId;

    private String issueTypeName;

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
