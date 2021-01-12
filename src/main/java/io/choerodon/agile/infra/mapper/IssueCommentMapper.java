package io.choerodon.agile.infra.mapper;

import io.choerodon.mybatis.common.BaseMapper;
import io.choerodon.agile.infra.dto.*;
import org.apache.ibatis.annotations.Param;

import java.util.List;


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
     * @param projectId 项目id
     * @param parentId 父评论id
     * @return 子评论
     */
    List<IssueCommentDTO> selectIssueCommentDesByParentId(@Param("projectId") Long projectId, @Param("parentId") Long parentId);
}
