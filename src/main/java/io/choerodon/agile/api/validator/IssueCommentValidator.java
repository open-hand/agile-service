package io.choerodon.agile.api.validator;


import com.alibaba.fastjson.JSONObject;
import io.choerodon.agile.api.vo.IssueCommentCreateVO;
import io.choerodon.agile.api.vo.IssueCommentReplayCreateVO;
import io.choerodon.agile.infra.dto.IssueCommentDTO;
import io.choerodon.agile.infra.mapper.IssueCommentMapper;
import io.choerodon.agile.infra.mapper.IssueMapper;
import io.choerodon.agile.infra.utils.EncryptionUtils;
import io.choerodon.core.exception.CommonException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * Created by HuangFuqiang@choerodon.io on 2019/7/8.
 * Email: fuqianghuang01@gmail.com
 */
@Component
public class IssueCommentValidator {

    @Autowired
    private IssueMapper issueMapper;
    @Autowired
    private IssueCommentMapper issueCommentMapper;

    private static final String COMMENT_ID = "commentId";

    public void verifyCreateData(IssueCommentCreateVO issueCommentCreateVO) {
        if (issueCommentCreateVO.getIssueId() == null) {
            throw new CommonException("error.IssueCommentRule.issueId");
        } else {
            if (issueMapper.selectByPrimaryKey(issueCommentCreateVO.getIssueId()) == null) {
                throw new CommonException("error.IssueCommentRule.issue");
            }
        }
    }

    public void verifyCreateReplayData(IssueCommentReplayCreateVO issueCommentReplayCreateVO) {
        if (issueCommentReplayCreateVO.getIssueId() == null) {
            throw new CommonException("error.IssueCommentRule.issueId");
        } else {
            if (issueMapper.selectByPrimaryKey(issueCommentReplayCreateVO.getIssueId()) == null) {
                throw new CommonException("error.IssueCommentRule.issue");
            }
        }
        if(issueCommentReplayCreateVO.getReplyToUserId() == null){
            throw new CommonException("error.IssueCommentRule.replyToUserId");
        }
        if (issueCommentReplayCreateVO.getParentId() == null) {
            throw new CommonException("error.IssueCommentRule.parentId");
        } else {
            if (issueCommentMapper.selectByPrimaryKey(issueCommentReplayCreateVO.getParentId()) == null) {
                throw new CommonException("error.IssueCommentRule.issueComment");
            }
        }
    }

    public void verifyUpdateData(Long projectId, JSONObject issueCommentUpdate) {
        if (issueCommentUpdate.get(COMMENT_ID) == null) {
            throw new CommonException("error.IssueCommentRule.commentId");
        }
        IssueCommentDTO issueCommentDTO = new IssueCommentDTO();
        issueCommentDTO.setCommentId(EncryptionUtils.decrypt(issueCommentUpdate.get(COMMENT_ID).toString(),
                EncryptionUtils.BLANK_KEY));
        issueCommentDTO.setProjectId(projectId);
        if (issueCommentMapper.selectByPrimaryKey(issueCommentDTO) == null) {
            throw new CommonException("error.IssueCommentRule.issueComment");
        }
    }

}
