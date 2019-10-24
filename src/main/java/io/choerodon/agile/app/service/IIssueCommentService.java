package io.choerodon.agile.app.service;

import io.choerodon.agile.infra.dto.IssueCommentDTO;

public interface IIssueCommentService {

    IssueCommentDTO createBase(IssueCommentDTO issueCommentDTO);

    IssueCommentDTO updateBase(IssueCommentDTO issueCommentDTO, String[] fieldList);

    int deleteBase(IssueCommentDTO issueCommentDTO);
}
