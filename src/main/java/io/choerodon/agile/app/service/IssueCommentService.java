package io.choerodon.agile.app.service;


import java.util.List;

import io.choerodon.agile.api.vo.*;

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
     * @return int
     */
    int deleteIssueComment(Long projectId, Long commentId);

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
     * @param issueCommentReplayCreateVO 创建issue评论回复对象
     * @return 创建的问题评论回复
     */
    IssueCommentVO createIssueCommentReplay(Long projectId, IssueCommentReplayCreateVO issueCommentReplayCreateVO);

    /**
     * 根据评论Id和项目id查询评论回复列表
     *
     * @param projectId 项目id
     * @param commentId 评论id
     * @return IssueCommentVO 评论回复列表
     */
    List<IssueCommentReplayVO> queryIssueCommentReplayList(Long projectId, Long commentId);

    /**
     * 删除issueComment及其评论
     *
     * @param projectId projectId
     * @param commentId commentId
     */
    void deleteIssueCommentReplay(Long projectId, Long commentId);
}