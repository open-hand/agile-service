package io.choerodon.agile.app.service;

import io.choerodon.agile.infra.dto.IssueAttachmentDTO;

public interface IIssueAttachmentService {

    IssueAttachmentDTO createBase(IssueAttachmentDTO issueAttachmentDTO);

    Boolean deleteBase(Long attachmentId);
}
