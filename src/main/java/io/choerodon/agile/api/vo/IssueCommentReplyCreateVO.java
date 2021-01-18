package io.choerodon.agile.api.vo;


import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

import io.choerodon.agile.infra.utils.StringUtil;

/**
 * @author dinghuang123@gmail.com
 */
public class IssueCommentReplyCreateVO {

    @ApiModelProperty(value = "问题id")
    @Encrypt
    private Long issueId;

    @ApiModelProperty(value = "父问题id")
    @Encrypt
    private Long parentId;

    @ApiModelProperty(value = "被回复人id")
    @Encrypt
    private Long replyToUserId;

    @ApiModelProperty(value = "评论内容")
    private String commentText;

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

    @Override
    public String toString() {
        return StringUtil.getToString(this);
    }
}
