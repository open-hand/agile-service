package io.choerodon.agile.api.vo;

import io.choerodon.agile.infra.dto.UserDTO;
import org.hzero.starter.keyencrypt.core.Encrypt;

/**
 * @author zhaotianxin
 * @date 2020-08-12 13:29
 */
public class StatusTransferSettingVO {
    @Encrypt
    private Long id;

    private Long projectId;
    @Encrypt
    private Long statusId;
    @Encrypt
    private Long issueTypeId;

    private String userType;
    @Encrypt
    private Long userId;

    private UserDTO user;

    private Boolean isVerifySubissueCompleted;

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
