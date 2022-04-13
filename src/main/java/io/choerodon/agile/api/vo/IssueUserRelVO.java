package io.choerodon.agile.api.vo;

import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

/**
 * @author huaxin.deng@hand-china.com
 * @since 2022/4/12
 */
public class IssueUserRelVO {

    @ApiModelProperty(value = "主键id")
    @Encrypt
    private Long id;

    @ApiModelProperty(value = "问题id")
    @Encrypt
    private Long issueId;

    @ApiModelProperty(value = "用户id")
    @Encrypt
    private Long userId;

    @ApiModelProperty(value = "用户类型")
    private String userType;

    @ApiModelProperty(value = "项目id")
    private Long projectId;

    @ApiModelProperty(value = "组织id")
    private Long organizationId;

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

    public Long getUserId() {
        return userId;
    }

    public void setUserId(Long userId) {
        this.userId = userId;
    }

    public String getUserType() {
        return userType;
    }

    public void setUserType(String userType) {
        this.userType = userType;
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

    @Override
    public String toString() {
        return "IssueUserRelVO{" +
                "id=" + id +
                ", issueId=" + issueId +
                ", userId=" + userId +
                ", userType='" + userType + '\'' +
                ", projectId=" + projectId +
                ", organizationId=" + organizationId +
                '}';
    }
}
