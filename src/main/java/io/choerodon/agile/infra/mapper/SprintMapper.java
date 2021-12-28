package io.choerodon.agile.infra.mapper;

import io.choerodon.agile.api.vo.SprintSearchVO;
import io.choerodon.agile.api.vo.SprintStartMessageVO;
import io.choerodon.agile.infra.dto.*;
import io.choerodon.agile.infra.dto.business.IssueSearchDTO;
import io.choerodon.mybatis.common.BaseMapper;
import org.apache.ibatis.annotations.Param;

import java.math.BigDecimal;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Created by jian_zhang02@163.com on 2018/5/15.
 */
public interface SprintMapper extends BaseMapper<SprintDTO> {
    List<SprintNameDTO> queryNameByOptions(@Param("projectIds") List<Long> projectIds,
                                           @Param("sprintStatusCodes") List<String> sprintStatusCodes);

    /**
     * 根据项目id和冲刺id查询冲刺
     *
     * @param projectId projectId
     * @param sprintId  sprintId
     * @return SprintDTO
     */
    SprintDTO queryByProjectIdAndSprintId(@Param("projectId") Long projectId, @Param("sprintId") Long sprintId);

    Boolean hasIssue(@Param("projectId") Long projectId, @Param("sprintId") Long sprintId);

    String queryMaxRank(@Param("projectId") Long projectId, @Param("sprintId") Long sprintId);

    SprintDTO getActiveSprint(@Param("projectId") Long projectId);

    int selectCountByStartedSprint(@Param("projectId") Long projectId);

    String queryMinRank(@Param("projectId") Long projectId, @Param("sprintId") Long sprintId);

    List<SprintSearchDTO> queryPlanSprint(@Param("projectId") Long projectId, @Param("issueIds") Set<Long> issueIds);

    int queryIssueCount(@Param("projectId") Long projectId, @Param("sprintId") Long sprintId);

    BigDecimal queryStoryPoint(@Param("statusIds") List<Long> statusIds, @Param("issueIds") List<Long> issueIds, @Param("projectId") Long projectId);

    SprintDTO queryLastSprint(@Param("projectId") Long projectId);

    List<SprintNameDTO> queryPlanSprintName(@Param("projectId") Long projectId);

    int queryDoneIssueCount(@Param("projectId") Long projectId, @Param("sprintId") Long sprintId);

    int queryNotDoneIssueCount(@Param("projectId") Long projectId, @Param("sprintId") Long sprintId);

    List<Long> queryIssueIdOrderByRankDesc(@Param("projectId") Long projectId, @Param("sprintId") Long sprintId);

    List<IssueNumDTO> queryParentsDoneUnfinishedSubtasks(@Param("projectId") Long projectId, @Param("sprintId") Long sprintId);

    List<Long> queryIssueIds(@Param("projectId") Long projectId, @Param("sprintId") Long sprintId);

    Set<Long> queryAssigneeIdsByIssueIds(@Param("projectId") Long projectId, @Param("issueIds") Set<Long> issueIds);

    List<Long> queryAllRankIssueIds(@Param("projectId") Long projectId, @Param("sprintId") Long sprintId);

    List<IssueSearchDTO> queryBacklogIssues(@Param("projectId") Long projectId, @Param("issueIds") List<Long> issueIds);

    SprintSearchDTO queryActiveSprintNoIssueIds(@Param("projectId") Long projectId);

    List<SprintSearchDTO> queryPlanSprintNoIssueIds(@Param("projectId") Long projectId);

    List<SprintDTO> queryUnClosedSprint(Long projectId);

    /**
     * 查询issueId没有关闭的所属冲刺id
     *
     * @param issueId   issueId
     * @param projectId projectId
     * @return sprintId
     */
    Long queryNotCloseSprintIdByIssueId(@Param("issueId") Long issueId, @Param("projectId") Long projectId);

    /**
     * 查询不在计划中的冲刺
     *
     * @param projectId projectId
     * @return SprintDTO
     */
    List<SprintDTO> queryNotPlanSprintByProjectId(@Param("projectId") Long projectId, @Param("startDate") Date startDate, @Param("endDate") Date endDate);

    Integer queryIssueCountInActiveBoard(@Param("projectId") Long projectId, @Param("sprintId") Long sprintId);

    List<Long> queryParentsDoneSubtaskUnDoneIds(@Param("projectId") Long projectId, @Param("sprintId") Long sprintId);

    List<Long> queryUnDoneSubOfParentIds(@Param("projectId") Long projectId, @Param("sprintId") Long sprintId);

    /**
     * 活跃冲刺的经办人统计信息
     *
     * @param projectId projectId
     * @param sprintId  sprintId
     * @return AssigneeIssueDTO
     */
    List<AssigneeIssueDTO> queryAssigneeIssueByActiveSprintId(@Param("projectId") Long projectId, @Param("sprintId") Long sprintId);


    /**
     * 根据issueids活跃冲刺的经办人统计信息
     *
     * @param projectId projectId
     * @param sprintId  sprintId
     * @return AssigneeIssueDTO
     */
    List<AssigneeIssueDTO> queryAssigneeIssueByActiveSprintIdAndIssueIds(@Param("projectId") Long projectId, @Param("sprintId") Long sprintId, @Param("issueIds") List<Long> issueIds);

    /**
     * 查询活跃冲刺的issue列表
     *
     * @param sprintId sprintId
     * @param issueIds issueIds
     * @return IssueSearchDTO
     */
    List<IssueSearchDTO> queryActiveSprintIssueSearchByIssueIds(@Param("projectId") Long projectId, @Param("issueIds") List<Long> issueIds, @Param("sprintId") Long sprintId);

    /**
     * 查询待办事项的所有冲刺中的所有用户
     *
     * @param projectId projectId
     * @return assigneeId
     */
    Set<Long> queryBacklogSprintAssigneeIds(@Param("projectId") Long projectId);

    List<SprintDTO> getSprintByProjectId(@Param("projectId") Long projectId);

    List<SprintDTO> selectNotDoneByProjectId(@Param("projectId") Long projectId);

    List<AssigneeIssueDTO> queryAssigneeIssueByPlanSprintId(@Param("sprintIds") Set<Long> sprintIds, @Param("projectId") Long projectId, @Param("advancedSearchArgs") Map<String, Object> advancedSearchArgs);

    /**
     * 根据对冲刺下面的用户进行分组统计
     *
     * @param projectId projectId
     * @param sprintIds  sprintIds
     * @return AssigneeIssueDTO
     */
    List<AssigneeIssueDTO> queryAssigneeIssueBySprintIds(@Param("projectId") Long projectId, @Param("sprintIds") List<Long> sprintIds);

    /**
     * 查询冲刺的故事点完成情况
     * @param projectId
     * @param sprintIds
     * @return
     */
    List<SprintSearchVO> queryStoryPointProgress(@Param("projectId")  Long projectId, @Param("sprintIds") List<Long> sprintIds);

    Long queryOutIssueId(@Param("projectId") Long projectId, @Param("sprintId") Long sprintId);

    List<SprintDTO> selectActiveSprintsByProjectIds(@Param("projectIds") Set<Long> projectIds);

    List querySprintIssue(@Param("projectId") Long projectId, @Param("sprintId") Long sprintId, @Param("advancedSearchArgs") Map<String, Object> advancedSearchArgs);

    List<SprintSearchDTO> queryPlanSprints(@Param("projectId") Long projectId);

    List<IssueCountDTO> querySprintIssueStoryPoints(@Param("projectId") Long projectId, @Param("sprintIds") List<Long> sprintIds, @Param("advancedSearchArgs") Map<String, Object> advancedSearchArgs);

    Set<Long> queryAssigneeIdsBySprintIds(@Param("projectId") Long projectId, @Param("sprintIds") List<Long> sprintIds, @Param("advancedSearchArgs") Map<String, Object> advancedSearchArgs);

    List<IssueCountDTO> selectCountBySprintIds(@Param("projectId") Long projectId, @Param("sprintIds") List<Long> sprintIds, @Param("advancedSearchArgs") Map<String, Object> advancedSearchArgs);

    SprintStartMessageVO selectSprintStartMessage(@Param("projectId") Long projectId, @Param("sprintId") Long sprintId);

    BigDecimal querySpentWorkTimeBySprintId(@Param("sprintId") Long sprint, @Param("projectId") Long projectId);

    List<Long> queryIssueIdsOrderByRank(@Param("projectId") Long projectId, @Param("sprintId") Long sprintId);
}
