package io.choerodon.agile.api.vo;

import io.choerodon.agile.infra.dto.UserDTO;
import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

/**
 * @author zhaotianxin
 * @date 2020-08-12 13:29
 */
public class StatusTransferSettingVO {
    @Encrypt
    @ApiModelProperty(value = "id")
    private Long id;
    @ApiModelProperty(value = "项目id")
    private Long projectId;
    @Encrypt
    @ApiModelProperty(value = "状态id")
    private Long statusId;
    @Encrypt
    @ApiModelProperty(value = "问题类型id")
    private Long issueTypeId;
    @ApiModelProperty(value = "用户类型")
    private String userType;
    @Encrypt
    @ApiModelProperty(value = "用户id")
    private Long userId;
    @ApiModelProperty(value = "用户")
    private UserDTO user;
    @ApiModelProperty(value = "是否验证订阅完成")
    private Boolean isVerifySubissueCompleted;
    @ApiModelProperty(value = "角色")
    private RoleVO role;
    @ApiModelProperty(value = "字段名称")
    private String fieldName;

    public String getFieldName() {
        return fieldName;
    }

    public void setFieldName(String fieldName) {
        this.fieldName = fieldName;
    }

    public RoleVO getRole() {
        return role;
    }

    public void setRole(RoleVO role) {
        this.role = role;
    }

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public Long getProjectId() {
        return projectId;
    }

    public void setProjectId(Long projectId) {
        this.projectId = projectId;
    }

    public Long getStatusId() {
        return statusId;
    }

    public void setStatusId(Long statusId) {
        this.statusId = statusId;
    }

    public Long getIssueTypeId() {
        return issueTypeId;
    }

    public void setIssueTypeId(Long issueTypeId) {
        this.issueTypeId = issueTypeId;
    }

    public String getUserType() {
        return userType;
    }

    public void setUserType(String userType) {
        this.userType = userType;
    }

    public Long getUserId() {
        return userId;
    }

    public void setUserId(Long userId) {
        this.userId = userId;
    }

    public UserDTO getUser() {
        return user;
    }

    public void setUser(UserDTO user) {
        this.user = user;
    }

    public Boolean getVerifySubissueCompleted() {
        return isVerifySubissueCompleted;
    }

    public void setVerifySubissueCompleted(Boolean verifySubissueCompleted) {
        isVerifySubissueCompleted = verifySubissueCompleted;
    }
}
