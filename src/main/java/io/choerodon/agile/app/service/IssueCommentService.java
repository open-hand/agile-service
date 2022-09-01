package io.choerodon.agile.app.service;


import java.util.List;

import io.choerodon.agile.api.vo.*;
import io.choerodon.core.domain.Page;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;

/**
 * 敏捷开发Issue评论
 *
 * @author dinghuang123@gmail.com
 * @since 2018-05-14 21:59:45
 */
public interface IssueCommentService {

    /**
     * 创建issue评论
     *
     * @param projectId             projectId
     * @param issueCommentCreateVO issueCommentCreateVO
     * @return IssueCommentVO
     */
    IssueCommentVO createIssueComment(Long projectId, IssueCommentCreateVO issueCommentCreateVO);

    /**
     * 更新issue评论
     *
     * @param issueCommentUpdateVO issueCommentUpdateVO
     * @param fieldList             fieldList
     * @param projectId             projectId
     * @return IssueCommentVO
     */
    IssueCommentVO updateIssueComment(IssueCommentUpdateVO issueCommentUpdateVO, List<String> fieldList, Long projectId);

    /**
     * 根据issueId和项目id查询IssueComment列表
     *
     * @param projectId projectId
     * @param issueId   issueId
     * @return IssueCommentVO
     */
    List<IssueCommentVO> queryIssueCommentList(Long projectId, Long issueId);

    /**
     * 删除issueComment
     *
     * @param projectId projectId
     * @param commentId commentId
     * @param self 是否删除自己的评论
     * @return int
     */
    int deleteIssueComment(Long projectId, Long commentId, boolean self);

    /**
     * 根据issueId删除评论
     *
     * @param issueId issueId
     * @return int
     */
    int deleteByIssueId(Long issueId);

    /**
     * 创建问题评论回复
     * @param projectId 项目id
     * @param issueCommentReplyCreateVO 创建issue评论回复对象
     * @return 创建的问题评论回复
     */
    IssueCommentVO createIssueCommentReply(Long projectId, IssueCommentReplyCreateVO issueCommentReplyCreateVO);

    /**
     * 根据评论Id和项目id查询评论回复列表
     *
     * @param projectId 项目id
     * @param commentId 评论id
     * @return IssueCommentVO 评论回复列表
     */
    List<IssueCommentReplyVO> queryIssueCommentReplyList(Long projectId, Long commentId);

    /**
     * 删除issueComment及其评论
     *
     * @param projectId projectId
     * @param commentId commentId
     * @param self 是否是删除自己的评论
     */
    void deleteIssueCommentReply(Long projectId, Long commentId, boolean self);

    /**
     * 分页查询issue下的评论
     * @param pageRequest 分页参数
     * @param issueId issueId
     * @param projectId 项目id
     * @return 问题评论
     */
    Page<IssueCommentVO> queryIssueCommentPage(PageRequest pageRequest, Long issueId, Long projectId);

    void copyIssueComments(Long projectId, Long issueId, Long newIssueId);
}