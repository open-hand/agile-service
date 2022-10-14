package io.choerodon.agile.api.vo;

import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

import java.util.Date;

/**
 * @author zhaotianxin
 * @date 2021-08-04 9:38
 */
public class StatusLinkageExecutionLogVO {

    @Encrypt
    @ApiModelProperty(value = "id")
    private Long id;

    @ApiModelProperty(value = "执行状态(LOOP/SUCCESS)")
    private String statusCode;

    @ApiModelProperty(value = "联动信息")
    private String content;

    @Encrypt
    @ApiModelProperty(value = "上一个issue")
    private Long preIssueId;
    @ApiModelProperty(value = "前一个问题信息")
    private IssueLinkVO preIssueInfo;

    @Encrypt
    @ApiModelProperty(value = "当前的issue")
    private Long curIssueId;
    @ApiModelProperty(value = "当前问题信息")
    private IssueLinkVO curIssueInfo;
    @ApiModelProperty(value = "项目id")
    private Long projectId;
    @ApiModelProperty(value = "组织id")
    private Long organizationId;
    @ApiModelProperty(value = "乐观锁")
    private Long objectVersionNumber;
    @ApiModelProperty(value = "创建日期")
    private Date creationDate;

    @ApiModelProperty(value = "备注(same_status/sub_bug/condition_limit)")
    private String remark;

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public String getStatusCode() {
        return statusCode;
    }

    public void setStatusCode(String statusCode) {
        this.statusCode = statusCode;
    }

    public String getContent() {
        return content;
    }

    public void setContent(String content) {
        this.content = content;
    }

    public Long getPreIssueId() {
        return preIssueId;
    }

    public void setPreIssueId(Long preIssueId) {
        this.preIssueId = preIssueId;
    }

    public Long getCurIssueId() {
        return curIssueId;
    }

    public void setCurIssueId(Long curIssueId) {
        this.curIssueId = curIssueId;
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

    public Long getObjectVersionNumber() {
        return objectVersionNumber;
    }

    public void setObjectVersionNumber(Long objectVersionNumber) {
        this.objectVersionNumber = objectVersionNumber;
    }

    public Date getCreationDate() {
        return creationDate;
    }

    public void setCreationDate(Date creationDate) {
        this.creationDate = creationDate;
    }

    public IssueLinkVO getPreIssueInfo() {
        return preIssueInfo;
    }

    public void setPreIssueInfo(IssueLinkVO preIssueInfo) {
        this.preIssueInfo = preIssueInfo;
    }

    public IssueLinkVO getCurIssueInfo() {
        return curIssueInfo;
    }

    public void setCurIssueInfo(IssueLinkVO curIssueInfo) {
        this.curIssueInfo = curIssueInfo;
    }

    public String getRemark() {
        return remark;
    }

    public void setRemark(String remark) {
        this.remark = remark;
    }
}
