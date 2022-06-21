package io.choerodon.agile.infra.mapper;

import io.choerodon.agile.api.vo.IssueCommentReplyVO;
import io.choerodon.mybatis.common.BaseMapper;
import io.choerodon.agile.infra.dto.*;

import org.apache.ibatis.annotations.Param;

import java.util.List;
import java.util.Set;


/**
 * 敏捷开发Issue评论
 *
 * @author dinghuang123@gmail.com
 * @since 2018-05-14 21:59:45
 */
public interface IssueCommentMapper extends BaseMapper<IssueCommentDTO> {

    /**
     * 根据issueId查询评论
     *
     * @param issueId   issueId
     * @param projectId projectId
     * @return IssueCommentDTO
     */
    List<IssueCommentDTO> queryIssueCommentList(@Param("projectId") Long projectId, @Param("issueId") Long issueId);

    void updateTransferProject(@Param("projectId") Long projectId, @Param("issueId") Long issueId, @Param("targetProjectId") Long targetProjectId);


    /**
     * 更新子评论的父id为0
     *
     * @param projectId 项目id
     * @param commentId 父评论id
     */
    void updateChildRecordParentNull(@Param("projectId") Long projectId, @Param("commentId") Long commentId);

    /**
     * 查询父评论的子评论
     *
     * @param projectId 项目id
     * @param parentId  父评论id
     * @return 子评论
     */
    List<IssueCommentDTO> selectIssueCommentDesByParentId(@Param("projectId") Long projectId, @Param("parentId") Long parentId);

    /**
     * 更新子评论的父id为新值
     *
     * @param projectId    项目id
     * @param oldCommentId 旧父评论id
     * @param newCommentId 新父评论id
     */
    void updateChildNewParent(@Param("projectId") Long projectId, @Param("oldCommentId") Long oldCommentId, @Param("newCommentId") Long newCommentId);

    /**
     * 查询评论下的回复
     *
     * @param commentIds 评论id
     * @param issueId    问题id
     * @param projectId  项目id
     * @return 评论下的回复
     */
    List<IssueCommentReplyVO> selectIssueCommentDesByParentIds(@Param("commentIds") Set<Long> commentIds, @Param("issueId") Long issueId, @Param("projectId") Long projectId);

    List<IssueCommentDTO> queryAllIssueCommentList(@Param("projectId") Long projectId, @Param("issueId") Long issueId);
}
