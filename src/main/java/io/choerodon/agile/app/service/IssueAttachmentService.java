package io.choerodon.agile.app.service;

import io.choerodon.agile.api.vo.IssueAttachmentCombineVO;
import io.choerodon.agile.api.vo.IssueAttachmentVO;
import io.choerodon.agile.infra.dto.IssueAttachmentDTO;
import io.choerodon.agile.infra.dto.TestCaseAttachmentDTO;

import javax.servlet.http.HttpServletRequest;
import java.util.List;

/**
 * Created by HuangFuqiang@choerodon.io on 2018/5/16.
 * Email: fuqianghuang01@gmail.com
 */
public interface IssueAttachmentService {

    List<IssueAttachmentVO> create(Long projectId, Long issueId, HttpServletRequest request);

    Boolean delete(Long projectId, Long issueAttachmentId);

    List<String> uploadForAddress(Long projectId, HttpServletRequest request);

    /**
     * 根据issueId删除附件
     *
     * @param issueId issueId
     * @return int
     */
    int deleteByIssueId(Long issueId);

    /**
     * 生成issueAttachment记录并生成日志（用于复制issue）
     *
     * @param projectId projectId
     * @param issueId   issueId
     * @param fileName  fileName
     * @param url       url
     * @return 创建的 issueAttachment
     */
    IssueAttachmentDTO dealIssue(Long projectId, Long issueId, String fileName, String url);

    List<TestCaseAttachmentDTO> migrateIssueAttachment();

    /**
     * 校验分片上传合并参数
     * @param issueAttachmentCombineVO 分片上传合并参数
     */
    void validCombineUpload(IssueAttachmentCombineVO issueAttachmentCombineVO);

    /**
     * 分片合并
     * @param projectId 项目id
     * @param issueAttachmentCombineVO 分片上传附件参数
     * @return 上传的文件
     */
    IssueAttachmentVO attachmentCombineUpload(Long projectId, IssueAttachmentCombineVO issueAttachmentCombineVO);

    void copyIssueAttachments(Long projectId, Long issueId, Long newIssueId);
}
