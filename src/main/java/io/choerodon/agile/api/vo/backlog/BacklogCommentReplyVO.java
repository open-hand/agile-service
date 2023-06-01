package io.choerodon.agile.api.vo.backlog;

import java.util.Date;

import io.swagger.annotations.ApiModelProperty;

import org.hzero.starter.keyencrypt.core.Encrypt;

/**
 * @author chihao.ran@hand-china.com
 * 2021/01/05 14:45
 */
public class BacklogCommentReplyVO {
    @ApiModelProperty("主键id")
    @Encrypt
    private Long id;
    @ApiModelProperty("用户id")
    @Encrypt
    private Long userId;
    @ApiModelProperty(value = "需求id")
    @Encrypt
    private Long backlogId;
    @ApiModelProperty("项目id")
    private Long projectId;
    @ApiModelProperty("评论内容")
    private String commentText;
    @ApiModelProperty("版本号")
    private Long objectVersionNumber;
    @ApiModelProperty("用户名称")
    private String userName;
    @ApiModelProperty("用户登录名称")
    private String userLoginName;
    @ApiModelProperty("用户真实名称")
    private String userRealName;
    @ApiModelProperty("用户图标")
    private String userImageUrl;
    @ApiModelProperty("最后更新时间")
    private Date lastUpdateDate;
    @ApiModelProperty(value = "父级id")
    @Encrypt
    private Long parentId;
    @ApiModelProperty("被回复的用户id")
    @Encrypt
    private Long replyToUserId;
    @ApiModelProperty("被回复的用户名称")
    private String replyToUserName;
    @ApiModelProperty("被回复的用户登录名称")
    private String replyToUserLoginName;
    @ApiModelProperty("被回复的用户真实名称")
    private String replyToUserRealName;
    @ApiModelProperty("被回复的用户图标")
    private String replyToUserImageUrl;

    public Long getParentId() {
        return parentId;
    }

    public void setParentId(Long parentId) {
        this.parentId = parentId;
    }

    public String getReplyToUserName() {
        return replyToUserName;
    }

    public void setReplyToUserName(String replyToUserName) {
        this.replyToUserName = replyToUserName;
    }

    public String getReplyToUserLoginName() {
        return replyToUserLoginName;
    }

    public void setReplyToUserLoginName(String replyToUserLoginName) {
        this.replyToUserLoginName = replyToUserLoginName;
    }

    public String getReplyToUserRealName() {
        return replyToUserRealName;
    }

    public void setReplyToUserRealName(String replyToUserRealName) {
        this.replyToUserRealName = replyToUserRealName;
    }

    public String getReplyToUserImageUrl() {
        return replyToUserImageUrl;
    }

    public void setReplyToUserImageUrl(String replyToUserImageUrl) {
        this.replyToUserImageUrl = replyToUserImageUrl;
    }

    public Long getReplyToUserId() {
        return replyToUserId;
    }

    public void setReplyToUserId(Long replyToUserId) {
        this.replyToUserId = replyToUserId;
    }

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public Long getUserId() {
        return userId;
    }

    public void setUserId(Long userId) {
        this.userId = userId;
    }

    public Long getBacklogId() {
        return backlogId;
    }

    public void setBacklogId(Long backlogId) {
        this.backlogId = backlogId;
    }

    public Long getProjectId() {
        return projectId;
    }

    public void setProjectId(Long projectId) {
        this.projectId = projectId;
    }

    public String getCommentText() {
        return commentText;
    }

    public void setCommentText(String commentText) {
        this.commentText = commentText;
    }

    public Long getObjectVersionNumber() {
        return objectVersionNumber;
    }

    public void setObjectVersionNumber(Long objectVersionNumber) {
        this.objectVersionNumber = objectVersionNumber;
    }

    public String getUserName() {
        return userName;
    }

    public void setUserName(String userName) {
        this.userName = userName;
    }

    public String getUserLoginName() {
        return userLoginName;
    }

    public void setUserLoginName(String userLoginName) {
        this.userLoginName = userLoginName;
    }

    public String getUserRealName() {
        return userRealName;
    }

    public void setUserRealName(String userRealName) {
        this.userRealName = userRealName;
    }

    public String getUserImageUrl() {
        return userImageUrl;
    }

    public void setUserImageUrl(String userImageUrl) {
        this.userImageUrl = userImageUrl;
    }

    public Date getLastUpdateDate() {
        return lastUpdateDate;
    }

    public void setLastUpdateDate(Date lastUpdateDate) {
        this.lastUpdateDate = lastUpdateDate;
    }

    @Override
    public String toString() {
        return "BacklogCommentRePlyVO{" +
                "id=" + id +
                ", userId=" + userId +
                ", backlogId=" + backlogId +
                ", projectId=" + projectId +
                ", commentText='" + commentText + '\'' +
                ", objectVersionNumber=" + objectVersionNumber +
                ", userName='" + userName + '\'' +
                ", userLoginName='" + userLoginName + '\'' +
                ", userRealName='" + userRealName + '\'' +
                ", userImageUrl='" + userImageUrl + '\'' +
                ", lastUpdateDate=" + lastUpdateDate +
                '}';
    }
}
