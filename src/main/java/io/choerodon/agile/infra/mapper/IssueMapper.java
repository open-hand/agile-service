package io.choerodon.agile.infra.mapper;

import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.api.vo.business.IssueVO;
import io.choerodon.agile.api.vo.report.CustomChartPointVO;
import io.choerodon.agile.infra.dto.*;
import io.choerodon.agile.infra.dto.business.IssueDetailDTO;
import io.choerodon.agile.infra.dto.business.IssueDTO;
import io.choerodon.agile.infra.dto.business.IssueSearchDTO;
import io.choerodon.mybatis.common.BaseMapper;

import org.apache.ibatis.annotations.Param;

import java.util.*;


/**
 * 敏捷开发Issue
 *
 * @author dinghuang123@gmail.com
 * @since 2018-05-14 20:30:48
 */
public interface IssueMapper extends BaseMapper<IssueDTO> {

    int removeFromSprint(@Param("projectId") Long projectId, @Param("sprintId") Long sprintId);

    /**
     * 根据issueId查询issueDetail
     *
     * @param projectId projectId
     * @param issueId   issueId
     * @return IssueDetailDTO
     */
    IssueDetailDTO queryIssueDetail(@Param("projectId") Long projectId, @Param("issueId") Long issueId);

    List<EpicDataDTO> queryEpicList(@Param("projectId") Long projectId);

    int batchIssueToVersion(@Param("projectId") Long projectId, @Param("versionId") Long versionId, @Param("issueIds") List<Long> issueIds, @Param("date") Date date, @Param("userId") Long userId);

    int batchIssueToEpic(@Param("projectId") Long projectId, @Param("epicId") Long epicId, @Param("issueIds") List<Long> issueIds);

    List<IssueSearchDTO> queryIssueByIssueIds(@Param("projectId") Long projectId, @Param("issueIds") List<Long> issueIds);

    /**
     * 根据项目id查询issue中的epic
     *
     * @param projectId projectId
     * @return IssueDTO
     */
    List<IssueEpicVO> queryIssueEpicSelectList(@Param("projectId") Long projectId,
                                               @Param("onlyUnCompleted") Boolean onlyUnCompleted,
                                               @Param("param") String param,
                                               @Param("epicIds") List<Long> epicIds);

    Integer batchRemoveFromVersion(@Param("projectId") Long projectId, @Param("issueIds") List<Long> issueIds);

    Integer batchRemoveFromVersionTest(@Param("projectId") Long projectId, @Param("issueIds") List<Long> issueIds);

    String queryRank(@Param("projectId") Long projectId, @Param("outsetIssueId") Long outsetIssueId);

    List<Long> queryIssueIdOrderByRankDesc(@Param("projectId") Long projectId, @Param("issueIds") List<Long> issueIds);

    List<Long> queryIssueIdOrderByRankAsc(@Param("projectId") Long projectId, @Param("issueIds") List<Long> issueIds);

    String queryRightRank(@Param("projectId") Long projectId, @Param("sprintId") Long sprintId, @Param("leftRank") String leftRank);

    String queryLeftRank(@Param("projectId") Long projectId, @Param("sprintId") Long sprintId, @Param("rightRank") String rightRank);

    /**
     * 查询issue子任务列表
     *
     * @param projectId projectId
     * @param issueId   issueId
     * @return IssueDTO
     */
    List<IssueDTO> queryIssueSubList(@Param("projectId") Long projectId, @Param("issueId") Long issueId);

    List<Long> queryIssueSubListByIssueIds(@Param("projectId") Long projectId, @Param("issueIds") List<Long> issueIds);

    void batchDeleteIssues(@Param("projectId") Long projectId, @Param("issueIds") List<Long> issueIds);

    /**
     * 把issueId对应的epic下的issue的epicId置为0
     *
     * @param projectId projectId
     * @param issueId   issueId
     * @return int
     */
    int batchUpdateIssueEpicId(@Param("projectId") Long projectId, @Param("issueId") Long issueId);

    List<IssueCountDTO> queryIssueCountByEpicIds(@Param("projectId") Long projectId, @Param("epicIds") List<Long> epicIds);

    List<IssueCountDTO> queryDoneIssueCountByEpicIds(@Param("projectId") Long projectId, @Param("epicIds") List<Long> epicIds);

    List<IssueCountDTO> queryNotEstimateIssueCountByEpicIds(@Param("projectId") Long projectId, @Param("epicIds") List<Long> epicIds);

    List<IssueCountDTO> queryTotalEstimateByEpicIds(@Param("projectId") Long projectId, @Param("epicIds") List<Long> epicIds);

    List<IssueLabelDTO> selectLabelNameByIssueId(@Param("issueId") Long issueId);

    List<Long> querySubTaskIds(@Param("projectId") Long projectId, @Param("sprintId") Long sprintId);

    int issueToDestinationByIds(@Param("projectId") Long projectId, @Param("sprintId") Long sprintId, @Param("issueIds") List<Long> issueIds, @Param("date") Date date, @Param("userId") Long userId);

    int batchUpdateIssueRank(@Param("projectId") Long projectId, @Param("moveIssues") List<MoveIssueDTO> moveIssues);

    List<Long> querySubIssueIds(@Param("projectId") Long projectId, @Param("issueIds") List<Long> issueIds);

    int removeIssueFromSprintByIssueIds(@Param("projectId") Long projectId, @Param("issueIds") List<Long> issueIds);

    List<Long> querySubIssueIdsByIssueId(@Param("projectId") Long projectId, @Param("issueId") Long issueId);

    int deleteIssueFromSprintByIssueId(@Param("projectId") Long projectId, @Param("issueId") Long issueId);

    SprintNameDTO queryActiveSprintNameByIssueId(@Param("issueId") Long issueId);

    List<SprintNameDTO> querySprintNameByIssueId(@Param("issueId") Long issueId);

    IssueDTO queryIssueSprintNotClosed(@Param("projectId") Long projectId, @Param("issueId") Long issueId);

    List queryIssueByOption(@Param("projectId") Long projectId,
                            @Param("activeSprintId") Long activeSprintId,
                            @Param("issueFilterParamVO") IssueFilterParamVO issueFilterParamVO,
                            @Param("typeCodes") List<String> typeCodes);

    /**
     * 根据参数查询issue列表，不对外开放
     *
     * @param projectId projectId
     * @param issueId   issueId
     * @param issueNum  issueNum
     * @param self      self
     * @param content   content
     * @return IssueNumDTO
     */
    List<IssueNumDTO> queryIssueByOptionForAgile(@Param("projectId") Long projectId,
                                                 @Param("issueId") Long issueId,
                                                 @Param("issueNum") String issueNum,
                                                 @Param("self") Boolean self,
                                                 @Param("content") String content,
                                                 @Param("excludeIssueIds") List<Long> excludeIssueIds);


    List<SprintNameDTO> querySprintNameByIssueIds(@Param("projectIds") List<Long> projectIds, @Param("issueIds") List<Long> issueIds);

    List<VersionIssueRelDTO> queryVersionIssueRelByIssueId(@Param("issueId") Long issueId);

    List<VersionIssueRelDTO> queryVersionNameByIssueIds(@Param("projectIds") List<Long> projectIds, @Param("issueIds") List<Long> issueIds, @Param("relationType") String relationType);

    List<LabelIssueRelDTO> queryLabelIssueByIssueIds(@Param("projectIds") List<Long> projectIds, @Param("issueIds") List<Long> issueIds);

    List<ComponentIssueRelDTO> queryComponentIssueByIssueIds(@Param("projectIds") List<Long> projectIds, @Param("issueIds") List<Long> issueIds);

    /**
     * 根据issueIds查询issueEpic信息
     *
     * @param projectId projectId
     * @param issueIds  issueIds
     * @return IssueDTO
     */
    List<IssueDTO> queryIssueEpicInfoByIssueIds(@Param("projectId") Long projectId, @Param("issueIds") List<Long> issueIds);

    IssueNumDTO queryIssueByIssueNumOrIssueId(@Param("projectId") Long projectId, @Param("issueId") Long issueId, @Param("issueNum") String issueNum);

    List<IssueInfoDTO> listByIssueIds(@Param("projectId") Long prjectId, @Param("issueIds") List<Long> issueIds);

    /**
     * 根据参数查询issueList提供给测试模块
     *
     * @param projectId          projectId
     * @param searchArgs         searchArgs
     * @param advancedSearchArgs advancedSearchArgs
     * @param otherArgs          otherArgs
     * @param contents           contents
     * @return IssueDTO
     */
    List<IssueDTO> listIssueWithoutSubToTestComponent(@Param("projectId") Long projectId,
                                                      @Param("searchArgs") Map<String, Object> searchArgs,
                                                      @Param("advancedSearchArgs") Map<String, Object> advancedSearchArgs,
                                                      @Param("otherArgs") Map<String, Object> otherArgs,
                                                      @Param("contents") List<String> contents);

    List<IssueDTO> listIssueWithLinkedIssues(@Param("projectId") Long projectId,
                                             @Param("searchArgs") Map<String, Object> searchArgs,
                                             @Param("advancedSearchArgs") Map<String, Object> advancedSearchArgs,
                                             @Param("otherArgs") Map<String, Object> otherArgs,
                                             @Param("contents") List<String> contents);

    List<IssueCreationNumDTO> queryIssueNumByTimeSlot(@Param("projectId") Long projectId,
                                                      @Param("typeCode") String typeCode,
                                                      @Param("date") Date date);

    /**
     * 查询issue和issue没有关闭的冲刺
     *
     * @param issueId issueId
     * @return IssueDTO
     */
    IssueDTO queryIssueWithNoCloseSprint(@Param("issueId") Long issueId);

    /**
     * 根据id查询epic
     *
     * @param issueId   issueId
     * @param projectId projectId
     * @return EpicDataDTO
     */
    EpicDataDTO queryEpicListByEpic(@Param("issueId") Long issueId, @Param("projectId") Long projectId);

    /**
     * 批量更新epic的排序
     *
     * @param sequence  sequence
     * @param projectId projectId
     * @param add       add
     * @param issueId   issueId
     * @return int
     */
    int batchUpdateSequence(@Param("sequence") Integer sequence, @Param("projectId") Long projectId, @Param("add") Integer add, @Param("issueId") Long issueId);

    /**
     * 查询epic的最大排序
     *
     * @param projectId projectId
     * @return Integer
     */
    Integer queryMaxEpicSequenceByProject(@Param("projectId") Long projectId);

    /**
     * 返回issue的详情列表（测试模块用）
     *
     * @param issueIds issueIds
     * @return IssueComponentDetailInfoDTO
     */
    List<IssueComponentDetailInfoDTO> listIssueWithoutSubDetailByIssueIds(@Param("issueIds") List<Long> issueIds);

    /**
     * 返回issueIds（测试模块用）
     *
     * @param projectId          projectId
     * @param searchArgs         searchArgs
     * @param advancedSearchArgs advancedSearchArgs
     * @param otherArgs          otherArgs
     * @param contents           contents
     * @return IssueComponentDetailInfoDTO
     */
    List<Long> listIssueIdsWithoutSubDetail(@Param("projectId") Long projectId, @Param("searchArgs") Map<String, Object> searchArgs,
                                            @Param("advancedSearchArgs") Map<String, Object> advancedSearchArgs,
                                            @Param("otherArgs") Map<String, Object> otherArgs, @Param("contents") List<String> contents);

    /**
     * 待办事项查询相关issue的issueIds，不包含已完成的issue
     *
     * @param projectId          projectId
     * @param userId             userId
     * @param advancedSearchArgs advancedSearchArgs
     * @param filterSql          filterSql
     * @return issueIds
     */
    List<Long> querySprintIssueIdsByCondition(@Param("projectId") Long projectId, @Param("userId") Long userId, @Param("advancedSearchArgs") Map<String, Object> advancedSearchArgs, @Param("filterSql") String filterSql, @Param("assigneeFilterIds") List<Long> assigneeFilterIds);

    /**
     * 待办事项查询相关issue的issueIds，包含已完成的issue
     *
     * @param projectId          projectId
     * @param userId             userId
     * @param advancedSearchArgs advancedSearchArgs
     * @param filterSql          filterSql
     * @return issueIds
     */
    List<IssueIdSprintIdVO> querySprintAllIssueIdsByCondition(@Param("projectId") Long projectId, @Param("userId") Long userId, @Param("advancedSearchArgs") Map<String, Object> advancedSearchArgs, @Param("filterSql") String filterSql, @Param("assigneeFilterIds") List<Long> assigneeFilterIds);

    Integer countUnResolveByProjectId(Long projectId);

    Integer countIssueByProjectId(Long projectId);

    List<Long> queryIssueIdsByOptions(@Param("projectId") Long projectId,
                                      @Param("advancedSearchArgs") Map<String, Object> advancedSearchArgs,
                                      @Param("otherArgs") Map<String, Object> otherArgs,
                                      @Param("contents") List<String> contents);

    List<UndistributedIssueDTO> queryUnDistributedIssues(Long projectId);

    List<UnfinishedIssueDTO> queryUnfinishedIssues(@Param("projectId") Long projectId,
                                                   @Param("assigneeId") Long assigneeId);

    /**
     * 查询当前issue的版本关系的版本id
     *
     * @param issueId   issueId
     * @param projectId projectId
     * @return versionIds 去重后的
     */
    List<Long> queryVersionIdsByIssueId(@Param("issueId") Long issueId, @Param("projectId") Long projectId);

    /**
     * 查询epic下的所有issue
     *
     * @param projectId projectId
     * @param epicId    epicId
     * @return IssueDTO
     */
    List<IssueBurnDownReportDTO> queryIssueByEpicId(@Param("projectId") Long projectId, @Param("epicId") Long epicId);

    /**
     * 查询版本下的所有issue
     *
     * @param projectId projectId
     * @param versionId versionId
     * @return IssueDTO
     */
    List<IssueBurnDownReportDTO> queryIssueByVersionId(@Param("projectId") Long projectId, @Param("versionId") Long versionId);

    String selectMaxRankByProjectId(@Param("projectId") Long projectId);

    Integer queryIssueIdsIsTest(@Param("projectId") Long projectId, @Param("issueIds") List<Long> issueIds);

    Integer queryIssueIdsIsNotTest(@Param("projectId") Long projectId, @Param("issueIds") List<Long> issueIds);

    /**
     * 查询epic信息
     *
     * @param issueId   issueId
     * @param projectId projectId
     * @return IssueDTO
     */
    IssueDTO queryEpicDetailByIssueId(@Param("issueId") Long issueId, @Param("projectId") Long projectId);

    /**
     * 查询epic相关信息
     *
     * @param issueId   issueId
     * @param projectId projectId
     * @return IssueDTO
     */
    IssueDTO queryEpicWithStatusByIssueId(@Param("issueId") Long issueId, @Param("projectId") Long projectId);

    /**
     * 性能不好，不要用
     *
     * @param projectId
     * @param searchVO
     * @param filterSql
     * @param assigneeFilterIds
     * @param sortMap
     * @param isTreeView
     * @return
     */
    @Deprecated
    List<IssueDTO> queryIssueIdsListWithSub(@Param("projectId") Long projectId,
                                            @Param("searchVO") SearchVO searchVO,
                                            @Param("filterSql") String filterSql,
                                            @Param("assigneeFilterIds") List<Long> assigneeFilterIds,
                                            @Param("sortMap") Map<String, Object> sortMap,
                                            @Param("isTreeView") Boolean isTreeView);


    List<IssueDTO> queryIssueIdsList(@Param("projectIds") Set<Long> projectIds,
                                     @Param("searchVO") SearchVO searchVO,
                                     @Param("filterSql") String filterSql,
                                     @Param("assigneeFilterIds") List<Long> assigneeFilterIds,
                                     @Param("sortMap") Map<String, Object> sortMap);

    List<IssueDTO> queryParentIssueIdsList(@Param("projectIds") Set<Long> projectIds,
                                           @Param("searchVO") SearchVO searchVO,
                                           @Param("filterSql") String filterSql,
                                           @Param("assigneeFilterIds") List<Long> assigneeFilterIds,
                                           @Param("sortMap") Map<String, Object> sortMap);


    List<IssueDTO> queryIssueListWithSubByIssueIds(@Param("issueIds") List<Long> issueIds,
                                                   @Param("childrenIds") Set<Long> childrenIds,
                                                   @Param("isExcelExported") boolean isExcelExported,
                                                   @Param("withSubIssues") boolean withSubIssues);

    List<IssueDTO> selectWithSubByIssueIds(@Param("projectIds") Set<Long> projectIds,
                                           @Param("issueIds") List<Long> issueIds,
                                           @Param("sortMap") Map<String, Object> sortMap,
                                           @Param("ganttDefaultOrder") boolean ganttDefaultOrder,
                                           @Param("dimension") String dimension);


    /**
     * 查询issueIds对应的issueDo
     *
     * @param issueIds issueIds
     * @return IssueDTO
     */
    List<IssueDTO> queryIssueByIssueIdsAndSubIssueIds(@Param("issueIds") List<Long> issueIds);

    /**
     * 查询issueIds对应的带当前冲刺的issueDO列表
     *
     * @param projectId projectId
     * @param issueIds  issueIds
     * @return IssueDTO
     */
    List<IssueDTO> queryIssueSprintNotClosedByIssueIds(@Param("projectId") Long projectId, @Param("issueIds") List<Long> issueIds);

    /**
     * 【内部接口】查询某个项目下某些应用类型处于某状态的issue有几个
     *
     * @param projectId
     * @param applyType
     * @param statusId
     * @return
     */
    Long querySizeByApplyTypeAndStatusId(@Param("projectId") Long projectId, @Param("applyType") String applyType, @Param("statusId") Long statusId);

    /**
     * 【内部接口】查询某个项目下某些问题类型下某应用类型的issue有几个
     *
     * @param projectId
     * @param applyType
     * @param issueTypeIds
     * @return
     */
    List<IssueDTO> queryByIssueTypeIdsAndApplyType(@Param("projectId") Long projectId, @Param("applyType") String applyType, @Param("issueTypeIds") List<Long> issueTypeIds);

    /**
     * 【内部调用】状态机方案变更后批量更新issue的状态匹配
     *
     * @param projectId
     * @param applyType
     * @param issueTypeId
     * @param oldStatusId
     * @param newStatusId
     */
    void updateIssueStatusByIssueTypeId(@Param("projectId") Long projectId, @Param("applyType") String applyType, @Param("issueTypeId") Long issueTypeId, @Param("oldStatusId") Long oldStatusId, @Param("newStatusId") Long newStatusId, @Param("userId") Long userId);

    /**
     * 查询某个状态下的issue信息包含是否已完成
     *
     * @param projectId   projectId
     * @param applyType   applyType
     * @param issueTypeId issueTypeId
     * @param statusId    statusId
     * @return IssueDTO
     */
    List<IssueDTO> queryIssueWithCompleteInfoByStatusId(@Param("projectId") Long projectId, @Param("applyType") String applyType, @Param("issueTypeId") Long issueTypeId, @Param("statusId") Long statusId);

    Long selectUnCloseSprintId(@Param("projectId") Long projectId, @Param("issueId") Long issueId);

    void updateStayDate(@Param("projectId") Long projectId, @Param("sprintId") Long sprintId, @Param("nowDate") Date nowDate);

    Long checkPriorityDelete(@Param("priorityId") Long priorityId, @Param("projectIds") List<Long> projectIds);

    void batchUpdateIssuePriority(@Param("priorityId") Long priorityId, @Param("changePriorityId") Long changePriorityId, @Param("userId") Long userId, @Param("projectIds") List<Long> projectIds);

    List<IssueDTO> queryIssuesByPriorityId(@Param("priorityId") Long priorityId, @Param("projectIds") List<Long> projectIds);

    List<Long> querySubBugIds(@Param("projectId") Long projectId, @Param("issueIds") List<Long> issueIds);

    List<Long> querySubBugIdsByIssueId(@Param("projectId") Long projectId, @Param("issueId") Long issueId);

    IssueNumDTO queryIssueByIssueNum(@Param("projectId") Long projectId, @Param("issueNum") String issueNum);

    void updateSubBugRelateIssueId(@Param("projectId") Long projectId, @Param("issueId") Long issueId);

    List<TestCaseDTO> migrateTestCase(@Param("projectId") Long projectId);

    List<Long> queryProjectIds();

    List<IssueDTO> listIssueInfoByIssueIds(@Param("projectId") Long projectId,
                                           @Param("issueIds") List<Long> issueIds,
                                           @Param("issueQueryVO") IssueQueryVO issueQueryVO);

    List<IssueDTO> queryChildrenIdByParentId(@Param("issueIds") List<Long> issueIds,
                                             @Param("projectIds") Set<Long> projectIds,
                                             @Param("searchVO") SearchVO searchVO,
                                             @Param("filterSql") String filterSql,
                                             @Param("assigneeFilterIds") List<Long> assigneeFilterIds,
                                             @Param("sortMap") Map<String, Object> sortMap);

    List<IssueDTO> queryStoryAndTaskByProjectId(@Param("projectId") Long projectId, @Param("searchVO") SearchVO searchVO);

    List<Long> selectIssueSubTaskAndSubBugIds(@Param("projectId") Long projectId, @Param("issueIds") List<Long> issueIds);

    /**
     * 查项目下issue的assignee_id
     *
     * @param projectIds
     * @return
     */
    Set<Long> selectUserIdsByProjectIds(@Param("projectIds") List<Long> projectIds);

    /**
     * 查项目下issue的reporter_id
     *
     * @param projectId
     * @return
     */
    Set<Long> selectReporterIdsByProjectId(@Param("projectId") Long projectId);

    /**
     * 查询个人未完成故事，任务和bug
     *
     * @param projectIds
     * @param userId
     * @param searchType
     * @return
     */
    List<IssueDTO> queryParentIssueByProjectIdsAndUserId(@Param("projectIds") List<Long> projectIds,
                                                         @Param("userId") Long userId,
                                                         @Param("searchType") String searchType,
                                                         @Param("searchVO") SearchVO searchVO);

    /**
     * 查询个人在所有子项目中未完成的问题
     *
     * @param projectIds
     * @param parentIssues
     * @param userId
     * @param searchType
     * @return
     */
    List<IssueDTO> listIssuesByParentIssueIdsAndUserId(@Param("projectIds") List<Long> projectIds,
                                                       @Param("parentIssues") List<Long> parentIssues,
                                                       @Param("userId") Long userId,
                                                       @Param("searchType") String searchType,
                                                       @Param("searchVO") SearchVO searchVO);

    List<IssueOverviewVO> selectIssueBysprint(@Param("projectId") Long projectId,
                                              @Param("sprintId") Long sprintId,
                                              @Param("statusSet") Set<String> statusSet);

    void updateStatusByStatusId(@Param("projectId") Long projectId, @Param("currentStatusId") Long currentStatusId, @Param("statusId") Long statusId);

    List<Long> selectStatusIdByIssueType(@Param("projectId") Long projectId, @Param("issueTypeId") Long issueTypeId);

    List<IssueCountDTO> countIssueTypeByStatusIds(@Param("projectId") Long projectId, @Param("statusIds") List<Long> statusIds);

    List<Long> selectIssueTypeIdsByStatusId(@Param("projectId") Long projectId, @Param("statusId") Long statusId);

    List<IssueDTO> querySubIssueByParentIssueId(@Param("projectId") Long projectId, @Param("parentIssueId") Long parentIssueId);

    List<IssueDTO> querySubIssueByIssueId(@Param("issueId") Long issueId);

    List<IssueDTO> querySubBugByIssueId(@Param("issueId") Long issueId);

    /**
     * 根据projectId和
     *
     * @param projectId
     * @param issueType
     * @param param
     * @return
     */
    List<IssueVO> listAvailableParents(@Param("projectId") Long projectId,
                                       @Param("issueType") String issueType,
                                       @Param("param") String param);

    List<IssueDTO> listMyStarIssuesByProjectIdsAndUserId(@Param("projectIds") List<Long> projectIds,
                                                         @Param("parentIssues") List<Long> parentIssues,
                                                         @Param("userId") Long userId,
                                                         @Param("searchVO") SearchVO searchVO);

    /**
     * 查询项目下未完成的issue，包含story, task和bug(不包含子缺陷)
     *
     * @param projectId
     * @return
     */
    List<IssueVO> listUndoneAvailableParents(@Param("projectId") Long projectId);

    IssueVO selectByIssueNum(@Param("projectId") Long projectId,
                             @Param("issueNum") String issueNum);

    IssueVO selectByIssueId(@Param("projectId") Long projectId,
                            @Param("issueId") Long issueId);

    List<Long> selectSubListByIssueIds(@Param("projectId") Long projectId, @Param("issueIds") List<Long> issueIds);

    List<GanttChartVO> selectActuatorCompletedDateByIssueIds(@Param("issueIds") List<Long> issueIds,
                                                             @Param("projectId") Long projectId);

    List<Long> selectSubIssueIds(@Param("issueId") Long issueId);

    List<IssueTypeCountVO> selectCountGroupByIssueTypeId(@Param("projectId") Long projectId,
                                                         @Param("issueTypeIds") Set<Long> issueTypeIds);

    /**
     * 更新问题最后更新信息（不含版本号更新
     *
     * @param issueId   问题id
     * @param projectId 项目id
     * @param userId    当前用户id
     */
    void updateIssueLastUpdateInfo(@Param("issueId") Long issueId, @Param("projectId") Long projectId, @Param("userId") Long userId);

    /**
     * 批量更新问题最后更新信息（不含版本号更新
     *
     * @param issueIds  要更新的问题id
     * @param projectId 项目id
     * @param userId    当前用户id
     */
    void batchUpdateIssueLastUpdateInfo(@Param("issueIds") List<Long> issueIds, @Param("projectId") Long projectId, @Param("userId") Long userId);

    List<Long> selectIdsByIssueTypeIdsAndProjectIds(@Param("projectIds") List<Long> projectIds,
                                                    @Param("issueTypeId") Long issueTypeId);

    /**
     * 根据项目id和日期查询活跃冲刺中的延期的问题
     *
     * @param projectIds
     * @param date
     */
    List<IssueDTO> selectDelayIssues(@Param("projectIds") Set<Long> projectIds,
                                     @Param("date") Date date);

    /**
     * 查询自定义报表数据点
     *
     * @param projectIds            项目id
     * @param searchVO             问题id
     * @param extendSearchVO
     * @param filterSql            filterSql
     * @param assigneeFilterIds    assigneeFilterIds
     * @param selectSql            查询sql
     * @param groupSql             分组sql
     * @param linkSql              表连接sql
     * @return 自定义报表数据点
     */
    List<CustomChartPointVO> selectCustomChartPointVO(
            @Param("projectIds") Set<Long> projectIds,
            @Param("searchVO") SearchVO searchVO,
            @Param("extendSearchVO") SearchVO extendSearchVO,
            @Param("filterSql") String filterSql,
            @Param("assigneeFilterIds") List<Long> assigneeFilterIds,
            @Param("selectSql") String selectSql,
            @Param("groupSql") String groupSql,
            @Param("linkSql") String linkSql);

    /**
     * 查询代办的问题
     * @param projectId
     * @param advancedSearchArgs
     * @return
     */
    List<Long> queryUnDoneIssues(@Param("projectId") Long projectId, @Param("statusIds") List<Long> statusIds, @Param("advancedSearchArgs") Map<String, Object> advancedSearchArgs);

    Set<Long> queryChildrenIds(@Param("projectId") Long projectId, @Param("statusIds") List<Long> statusIds , @Param("parentIssueIds") List<Long> parentIssueIds, @Param("advancedSearchArgs") Map<String, Object> advancedSearchArgs);

    List<IssueDailyWorkVO> selectDailyWorkIssues(@Param("projectIds") Set<Long> projectIds);

    List<Long> queryUnDoneAllIssues(@Param("projectId") Long projectId, @Param("statusIds") List<Long> statusIds, @Param("advancedSearchArgs") Map<String, Object> advancedSearchArgs);

    IssueCountDTO querySubIssueCount(@Param("projectId") Long projectId, @Param("parentIssueId") Long parentIssueId);

    List<IssueEpicVO> queryIssueEpicByIds(@Param("projectId")Long projectId, @Param("epicIds") List<Long> epicIds);

    List<IssueDTO> listIssueWithEpicId(@Param("projectIds") Set<Long> projectIds,
                                       @Param("issueIds") List<Long> issueIds);

    List<IssueDTO> selectEpicByLinkIssueIds(@Param("projectIds") Set<Long> projectIds,
                                            @Param("issueIds") List<Long> issueIds);

    Set<Long> selectAssigneeIdByIssueIds(@Param("projectId") Long projectId,
                                         @Param("issueIds") List<Long> issueIds);

    List<WorkItemVO> listWorkCalenderParentIssue(@Param("projectIds") List<Long> projectIds, @Param("userId") Long userId, @Param("workItemSearchVO") WorkItemSearchVO workItemSearchVO);

    List<CountVO> countWorkCalenderSubIssueProgress(@Param("projectIds") List<Long> projectIds, @Param("userId") Long userId, @Param("issueIds") List<Long> issueIds);

    List<WorkItemVO> queryAssigneeIssueList(@Param("projectIds") List<Long> projectIds, @Param("userId") Long userId, @Param("workItemSearchVO") WorkItemSearchVO workItemSearchVO);

    Set<Long> queryAssigneeIdsBySearchVO(@Param("projectIds") Set<Long> projectIds,
                                         @Param("searchVO") SearchVO searchVO,
                                         @Param("filterSql") String filterSql,
                                         @Param("assigneeFilterIds") List<Long> assigneeFilterIds);

    List<IssueDTO> selectConflictEstimatedTime(@Param("projectIds") Set<Long> projectIds,
                                               @Param("assigneeIds") Set<Long> assigneeIds,
                                               @Param("searchVO") SearchVO searchVO,
                                               @Param("filterSql") String filterSql,
                                               @Param("assigneeFilterIds") List<Long> assigneeFilterIds,
                                               @Param("sortMap") Map<String, Object> sortMap);

    List<Long> selectCompletedSubIssue(@Param("projectId") Long projectId, @Param("issueIds") List<Long> issueIds);

    void updateSubIssueHistoryCompleted(@Param("projectId") Long projectId, @Param("issueIds") List<Long> issueIds);

    /**
     * 查询issueIds中的子任务和子Bug
     * @param projectId
     * @param issueIds
     * @return
     */
    List<IssueDTO> queryChildrenIssue(@Param("projectId") Long projectId, @Param("issueIds") List<Long> issueIds);

    IssueCopyLinkContents queryIssueLinkContents(@Param("projectId") Long projectId, @Param("issueId") Long issueId);
}