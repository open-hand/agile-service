package io.choerodon.agile.infra.mapper;

import org.apache.ibatis.annotations.Param;

import java.util.List;
import java.util.Set;

import io.choerodon.agile.api.vo.ColumnWithMaxMinNumVO;
import io.choerodon.agile.api.vo.IssueCountStatusVO;
import io.choerodon.agile.api.vo.SearchVO;
import io.choerodon.agile.api.vo.event.RemoveStatusWithProject;
import io.choerodon.agile.infra.dto.*;
import io.choerodon.mybatis.common.BaseMapper;

/**
 * Created by HuangFuqiang@choerodon.io on 2018/5/14.
 * Email: fuqianghuang01@gmail.com
 */
public interface BoardColumnMapper extends BaseMapper<BoardColumnDTO> {

    List<ColumnAndIssueDTO> selectColumnsByBoardId(@Param("projectIds") Set<Long> projectIds,
                                                   @Param("boardId") Long boardId,
                                                   @Param("sprintId") Long sprintId,
                                                   @Param("filterSql") String filterSql,
                                                   @Param("searchVO") SearchVO searchVO,
                                                   @Param("assigneeFilterIds") List<Long> assigneeFilterIds,
                                                   @Param("userId") Long userId);

    void columnSort(@Param("boardId") Long boardId,
                    @Param("sequence") Integer sequence,
                    @Param("originSequence") Integer originSequence);

    void columnSortDesc(@Param("boardId") Long boardId,
                        @Param("sequence") Integer sequence,
                        @Param("originSequence") Integer originSequence);

    void updateColumnCategory(@Param("boardId") Long boardId,
                              @Param("columnNum") Integer columnNum);

    void updateColumnColor(@Param("boardId") Long boardId,
                           @Param("columnNum") Integer columnNum);

    List queryColumnStatusRelByProjectId(@Param("projectId") Long projectId);

    void updateMaxAndMinNum(@Param("columnInfo") ColumnWithMaxMinNumVO columnWithMaxMinNumVO);

    List<BoardColumnDTO> selectByBoardIdOrderBySequence(@Param("boardId") Long boardId);

    void updateSequenceWhenDelete(@Param("boardId") Long boardId, @Param("sequence") Integer sequence);

    /**
     * 根据列id获取列对象
     *
     * @param columnIds columnIds
     * @return ColumnDTO
     */
    List<ColumnDTO> queryColumnByColumnIds(@Param("columnIds") List<Long> columnIds);

    /**
     * 根据issueIds集合获取ColumnAndIssueDO
     *
     * @param issueIds issueIds
     * @param boardId  boardId
     * @return ColumnAndIssueDTO
     */
    List<ColumnAndIssueDTO> queryColumnsByIssueIds(@Param("issueIds") List<Long> issueIds, @Param("boardId") Long boardId);

    List<EpicIdWithNameDTO> selectEpicBatchByIds(@Param("epicIds") List<Long> epicIds);

    /**
     * 根据看板id和projectId查询列idList
     *
     * @param boardId   boardId
     * @param projectId projectId
     * @return Long
     */
    List<Long> queryColumnIdsByBoardId(@Param("boardId") Long boardId, @Param("projectId") Long projectId);

    /**
     * 批量删除列和状态的关系（包含状态）
     *
     * @param removeStatusWithProjects removeStatusWithProjects
     */
    void batchDeleteColumnAndStatusRel(@Param("removeStatusWithProjects") List<RemoveStatusWithProject> removeStatusWithProjects);

    List<Long> sortAndJudgeCompleted(@Param("projectId") Long projectId, @Param("parentIds") List<Long> parentIds);

    List<ParentIssueDTO> queryParentIssuesByIds(@Param("projectId") Long projectId, @Param("parentIds") List<Long> parentIds);

    List<ColumnIssueNumDTO> getAllColumnNum(@Param("projectId") Long projectId,
                                            @Param("boardId") Long boardId,
                                            @Param("sprintId") Long sprintId,
                                            @Param("columnConstraint") String columnConstraint);

    List<BoardColumnStatusRelDTO> queryRelByColumnIds(@Param("columnIds") List<Long> columnIds);

    List<BoardColumnStatusRelDTO> selectByStatusId(@Param("projectId") Long projectId, @Param("statusId") Long statusId);

    void deleteByStatusId(@Param("projectId") Long projectId, @Param("statusId") Long statusId);

    Set<Long> queryStatusByBoardId(@Param("projectId") Long projectId, @Param("boardId") Long boardId);

    /**
     * 查询看板列信息
     * @param projectId 项目id
     * @param boardId 看板id
     * @return 看板列信息
     */
    List<ColumnAndIssueDTO> selectColumnInfoByBoardId(@Param("projectId") Long projectId, @Param("boardId") Long boardId);

    /**
     * 查询看板issue
     * @param projectIds 项目id
     * @param sprintId 冲刺id
     * @param filterSql 筛选sql
     * @param searchVO 查询条件
     * @param assigneeFilterIds 经办人筛选sql
     * @param userId 用户id
     * @param statusIds 状态id
     * @return 看板issue
     * @see BoardColumnMapper#selectBoardIssueV2(Set, Long, String, String, Long, Set, boolean)
     */
    @Deprecated
    List<IssueForBoardDO> selectBoardIssue(@Param("projectIds") Set<Long> projectIds, @Param("sprintId") Long sprintId, @Param("filterSql") String filterSql, @Param("searchVO") SearchVO searchVO, @Param("assigneeFilterIds") List<Long> assigneeFilterIds, @Param("userId") Long userId, @Param("statusIds") Set<Long> statusIds);

    List<IssueForBoardDO> selectBoardIssueV2(@Param("projectIds") Set<Long> projectIds,
                                             @Param("sprintId") Long sprintId,
                                             @Param("quickFilterSql") String quickFilterSql,
                                             @Param("advancedSql") String advancedSql,
                                             @Param("userId") Long userId,
                                             @Param("statusIds") Set<Long> statusIds,
                                             @Param("isHidePreSprintDoneSubissue") boolean isHidePreSprintDoneSubissue);

    /**
     * 查询看板issue数量
     * @param statusIds 状态id
     * @param projectId 项目id
     * @param sprintId 冲刺id
     * @param columnConstraint 列约束
     * @return 看板issue数量
     */
    List<IssueCountStatusVO> getColumnNumByStatus(@Param("statusIds") Set<Long> statusIds, @Param("projectId") Long projectId, @Param("sprintId") Long sprintId, @Param("columnConstraint") String columnConstraint);
}
