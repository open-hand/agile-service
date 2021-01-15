package io.choerodon.agile.app.service;

import io.choerodon.agile.infra.dto.IssueCommentDTO;

public interface IIssueCommentService {

    IssueCommentDTO createBase(IssueCommentDTO issueCommentDTO);

    IssueCommentDTO updateBase(IssueCommentDTO issueCommentDTO, String[] fieldList);

    int deleteBase(IssueCommentDTO issueCommentDTO);

    /**
     * 删除评论及其回复
     * @param issueCommentDTO 要删除的及其回复的评论
     */
    void deleteBaseReply(IssueCommentDTO issueCommentDTO);
}
