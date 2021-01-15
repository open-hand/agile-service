package io.choerodon.agile.api.vo;


import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

import java.util.Date;
import java.util.List;

import io.choerodon.agile.infra.utils.StringUtil;

/**
 * 敏捷开发Issue评论
 *
 * @author dinghuang123@gmail.com
 * @since 2018-05-14 21:59:45
 */
public class IssueCommentReplyVO {

    @ApiModelProperty(value = "主键id")
    @Encrypt
    private Long commentId;

    @ApiModelProperty(value = "用户id")
    @Encrypt
    private Long userId;

    @ApiModelProperty(value = "问题id")
    @Encrypt
    private Long issueId;

    @ApiModelProperty(value = "评论内容")
    private String commentText;

    @ApiModelProperty(value = "版本号")
    private Long objectVersionNumber;

    @ApiModelProperty(value = "项目id")
    private Long projectId;

    @ApiModelProperty(value = "用户名称")
    private String userName;

    @ApiModelProperty(value = "用户登录名称")
    private String userLoginName;

    @ApiModelProperty(value = "用户真实名称")
    private String userRealName;

    @ApiModelProperty(value = "用户图标")
    private String userImageUrl;

    @ApiModelProperty(value = "最后更新时间")
    private Date lastUpdateDate;

    @ApiModelProperty("父评论id")
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

    @ApiModelProperty(value = "附件列表")
    private List<IssueAttachmentVO> issueAttachmentVOList;

    public Long getCommentId() {
        return commentId;
    }

    public void setCommentId(Long commentId) {
        this.commentId = commentId;
    }

    public Long getUserId() {
        return userId;
    }

    public void setUserId(Long userId) {
        this.userId = userId;
    }

    public Long getIssueId() {
        return issueId;
    }

    public void setIssueId(Long issueId) {
        this.issueId = issueId;
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

    public Long getProjectId() {
        return projectId;
    }

    public void setProjectId(Long projectId) {
        this.projectId = projectId;
    }

    public List<IssueAttachmentVO> getIssueAttachmentVOList() {
        return issueAttachmentVOList;
    }

    public void setIssueAttachmentVOList(List<IssueAttachmentVO> issueAttachmentVOList) {
        this.issueAttachmentVOList = issueAttachmentVOList;
    }

    public String getUserName() {
        return userName;
    }

    public void setUserName(String userName) {
        this.userName = userName;
    }

    public Date getLastUpdateDate() {
        return lastUpdateDate;
    }

    public void setLastUpdateDate(Date lastUpdateDate) {
        this.lastUpdateDate = lastUpdateDate;
    }

    public String getUserImageUrl() {
        return userImageUrl;
    }

    public void setUserImageUrl(String userImageUrl) {
        this.userImageUrl = userImageUrl;
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

    public Long getParentId() {
        return parentId;
    }

    public void setParentId(Long parentId) {
        this.parentId = parentId;
    }

    public Long getReplyToUserId() {
        return replyToUserId;
    }

    public void setReplyToUserId(Long replyToUserId) {
        this.replyToUserId = replyToUserId;
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

    @Override
    public String toString() {
        return StringUtil.getToString(this);
    }

}