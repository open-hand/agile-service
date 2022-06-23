package io.choerodon.agile.app.service;

import java.util.List;
import java.util.Map;
import java.util.Set;

import com.alibaba.fastjson.JSONObject;

import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.api.vo.business.*;
import io.choerodon.agile.infra.dto.*;
import io.choerodon.agile.infra.dto.business.IssueConvertDTO;
import io.choerodon.agile.infra.dto.business.IssueDTO;
import io.choerodon.agile.infra.dto.business.IssueDetailDTO;
import io.choerodon.agile.infra.mapper.IssueMapper;
import io.choerodon.agile.infra.statemachineclient.dto.InputDTO;
import io.choerodon.core.domain.Page;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;


/**
 * 敏捷开发Issue
 *
 * @author dinghuang123@gmail.com
 * @since 2018-05-14 20:30:48
 */
public interface IssueService {

    void setIssueMapper(IssueMapper issueMapper);

    IssueVO queryIssueCreate(Long projectId, Long issueId);

    void handleInitIssue(IssueConvertDTO issueConvertDTO, Long statusId, ProjectInfoDTO projectInfoDTO);

    void afterCreateIssue(Long issueId, IssueConvertDTO issueConvertDTO, IssueCreateVO issueCreateVO, ProjectInfoDTO projectInfoDTO);

    void afterCreateSubIssue(Long issueId, IssueConvertDTO subIssueConvertDTO, IssueSubCreateVO issueSubCreateVO, ProjectInfoDTO projectInfoDTO);

    /**
     * 查询单个issue
     *
     * @param projectId projectId
     * @param issueId   issueId
     * @return IssueVO
     */
    IssueVO queryIssue(Long projectId, Long issueId, Long organizationId);

    /**
     * 分页过滤查询issueList（包含子任务）
     *
     * @param projectId   projectId
     * @param searchVO   searchVO
     * @param pageRequest pageRequest
     * @return IssueListVO
     */
    Page<IssueListFieldKVVO> listIssueWithSub(Long projectId, SearchVO searchVO, PageRequest pageRequest, Long organizationId);

    Page<Long> pagedQueryByTreeView(PageRequest pageRequest,
                                    Set<Long> projectIds,
                                    SearchVO searchVO,
                                    String searchSql,
                                    Map<String, Object> sortMap,
                                    boolean isTreeView);

    List<Long> listByTreeView(Set<Long> projectIds,
                              SearchVO searchVO,
                              String searchSql,
                              Map<String, Object> sortMap,
                              boolean isTreeView);

    List<EpicDataVO> listEpic(Long projectId);

    /**
     * 更新issue
     *
     * @param projectId      projectId
     * @param issueUpdateVO issueUpdateVO
     * @param fieldList      fieldList
     * @return IssueVO
     */
    IssueVO updateIssue(Long projectId, IssueUpdateVO issueUpdateVO, List<String> fieldList);

    /**
     * 更新issue的状态
     *
     * @param projectId
     * @param issueId
     * @param transformId
     * @return
     */
    IssueVO updateIssueStatus(Long projectId, Long issueId, Long transformId, Long objectVersionNumber, String applyType);

    /**
     * 更新issue的状态
     *
     * @param projectId
     * @param issueId
     * @param transformId
     * @return
     */
    IssueVO updateIssueStatus(Long projectId, Long issueId, Long transformId, Long objectVersionNumber,
                              String applyType, IssueDTO triggerIssue, boolean autoTranferFlag);

    /**
     * 执行状态机自定义流转
     *
     * @param projectId
     * @param issueId
     * @param applyType
     * @param influenceIssueIds
     * @return
     */
    IssueVO doStateMachineCustomFlow(Long projectId, Long issueId, String applyType, Set<Long> influenceIssueIds, TriggerCarrierVO triggerCarrierVO);

    /**
     * 更新issue自己的字段
     *
     * @param issueUpdateVO
     * @param fieldList
     * @param projectId
     */
    void handleUpdateIssue(IssueUpdateVO issueUpdateVO, List<String> fieldList, Long projectId, Long issueId);

    /**
     * 删除issue
     *
     * @param projectId projectId
     * @param issueId   issueId
     * @return int
     */
    void deleteIssue(Long projectId, Long issueId);

    void batchDeleteIssues(Long projectId, List<Long> issueIds);

    void batchDeleteIssuesAgile(Long projectId, List<Long> issueIds);

    void handleInitSubIssue(IssueConvertDTO subIssueConvertDTO, Long statusId, ProjectInfoDTO projectInfoDTO);

    IssueSubVO queryIssueSubByCreate(Long projectId, Long issueId);

    List<IssueSearchVO> batchIssueToVersion(Long projectId, Long versionId, List<Long> issueIds);

    void batchIssueToVersionTest(Long projectId, Long versionId, List<Long> issueIds);

    List<IssueSearchVO> batchIssueToEpic(Long projectId, Long epicId, List<Long> issueIds);

    List<IssueSearchVO> batchIssueToSprint(Long projectId, Long sprintId, MoveIssueVO moveIssueVO);

    /**
     * 根据项目id查询epic
     *
     * @param projectId projectId
     * @return IssueEpicVO
     */
    Page<IssueEpicVO> listEpicSelectData(Long projectId, PageRequest pageRequest, Boolean onlyUnCompleted, String param, List<Long> epicIds);

    /**
     * 查询单个子任务信息
     *
     * @param projectId projectId
     * @param issueId   issueId
     * @return IssueSubVO
     */
    IssueSubVO queryIssueSub(Long projectId, Long organizationId, Long issueId);

    /**
     * 更改issue类型
     *
     * @param issueConvertDTO             issueConvertDTO
     * @param issueUpdateTypeVO issueUpdateTypeVO
     * @param organizationId
     * @param projectId
     * @return IssueVO
     */
    IssueVO updateIssueTypeCode(IssueConvertDTO issueConvertDTO, IssueUpdateTypeVO issueUpdateTypeVO, Long organizationId, Long projectId);

    /**
     * 通过项目id和issueId查询issueE
     *
     * @param projectId projectId
     * @param issueId   issueId
     * @return IssueConvertDTO
     */
    IssueConvertDTO queryIssueByProjectIdAndIssueId(Long projectId, Long issueId);

    Page<IssueNumVO> queryIssueByOption(Long projectId, IssueFilterParamVO issueFilterParamVO , PageRequest pageRequest);

    /**
     * 根据issueId复制一个issue
     *
     * @param projectId        projectId
     * @param issueId          issueId
     * @param copyConditionVO copyConditionVO
     */
    void cloneIssueByIssueId(Long projectId, Long issueId, CopyConditionVO copyConditionVO, Long organizationId, String applyType);

    List<String> handlerCopyRequirePredefinedField(Object object, JSONObject predefinedFields);

    void handleCopyPredefinedFields(Long origanizationId, IssueDetailDTO issueDetailDTO, List<String> predefinedFieldNames);

    /**
     * 根据issueId转换为子任务
     *
     * @param projectId             projectId
     * @param issueTransformSubTask issueTransformSubTask
     * @return IssueSubVO
     */
    IssueSubVO transformedSubTask(Long projectId, Long organizationId, IssueTransformSubTask issueTransformSubTask);

    /**
     * 子任务转换为任务
     *
     * @param issueConvertDTO
     * @param issueTransformTask
     * @param organizationId
     * @return
     */
    IssueVO transformedTask(IssueConvertDTO issueConvertDTO, IssueTransformTask issueTransformTask, Long organizationId);

    List<IssueInfoVO> listByIssueIds(Long projectId, List<Long> issueIds);

    /**
     * 参数查询issueList提供给测试模块
     *
     * @param projectId   projectId
     * @param searchVO   searchVO
     * @param pageRequest pageRequest
     * @return IssueListVO
     */
    Page<IssueListTestVO> listIssueWithoutSubToTestComponent(Long projectId, SearchVO searchVO, PageRequest pageRequest, Long organizationId);

    Page<IssueListTestWithSprintVersionVO> listIssueWithLinkedIssues(Long projectId, SearchVO searchVO, PageRequest pageRequest, Long organizationId);

    List<IssueCreationNumVO> queryIssueNumByTimeSlot(Long projectId, String typeCode, Integer timeSlot);

    /**
     * 参数查询issue列表，不对外开放
     *
     * @param projectId   projectId
     * @param issueId     issueId
     * @param issueNum    issueNum
     * @param self        self
     * @param content     content
     * @param pageRequest pageRequest
     * @return IssueNumVO
     */
    Page<IssueNumVO> queryIssueByOptionForAgile(Long projectId, Long issueId, String issueNum,
                                                    Boolean self, String content, PageRequest pageRequest, List<Long> excludeIssueIds);

    /**
     * 拖动epic
     *
     * @param projectId       projectId
     * @param epicSequenceVO epicSequenceVO
     * @return EpicDataVO
     */
    EpicDataVO dragEpic(Long projectId, EpicSequenceVO epicSequenceVO);

    IssueVO issueParentIdUpdate(Long projectId, IssueUpdateParentIdVO issueUpdateParentIdVO);

    JSONObject countUnResolveByProjectId(Long projectId);

    List<Long> queryIssueIdsByOptions(Long projectId, SearchVO searchVO);

    Page<UndistributedIssueVO> queryUnDistributedIssues(Long projectId, PageRequest pageable);

    List<UnfinishedIssueVO> queryUnfinishedIssues(Long projectId, Long assigneeId);

    /**
     * 查询用户故事地图泳道
     *
     * @param projectId projectId
     * @return String
     */
    String querySwimLaneCode(Long projectId);

    /**
     * 批量把issue根据冲刺判断更新为初始状态
     *
     * @param projectId    projectId
     * @param moveIssueIds moveIssueIds
     * @param sprintId     sprintId
     */
    void batchHandleIssueStatus(Long projectId, List<Long> moveIssueIds, Long sprintId);

    /**
     * 处理高级搜索中的用户搜索
     *
     * @param searchVO searchVO
     * @param projectId projectId
     */
    Boolean handleSearchUser(SearchVO searchVO, Long projectId);

    Boolean checkEpicName(Long projectId, String epicName, Long epicId);

    /**
     * 根据projectId按批次迁移数据
     */
    List<TestCaseDTO> migrateTestCaseByProjectId(Long projectId);

    /**
     * 查询issue中的projecid
     */
    List<Long> queryProjectIds();

    List<IssueLinkVO> queryIssueByIssueIds(Long projectId, List<Long> issueIds);

    Page<IssueListFieldKVVO> queryStoryAndTask(Long projectId, PageRequest pageRequest, SearchVO searchVO);

    /**
     * 分页查询项目下的项目成员和分配过问题的用户
     *
     * @param pageRequest
     * @param projectId
     * @param param
     * @return
     */
    Page<UserDTO> pagingQueryUsers(PageRequest pageRequest, Long projectId, String param, Set<Long> ignoredUserIds);

    /**
     * 分页查询项目下的项目成员和分配过问题的报告人
     *
     * @param pageRequest
     * @param projectId
     * @param param
     * @return
     */
    Page<UserDTO> pagingQueryReporters(PageRequest pageRequest, Long projectId, String param, Set<Long> ignoredUserIds);

    /**
     * 删除自己的创建的Issue
     * @param projectId
     * @param issueId
     */
    void deleteSelfIssue(Long projectId, Long issueId);

    /**
     * 个人工作台查询代办事项
     * @param organizationId
     * @param projectId
     * @param pageRequest
     * @param workBenchIssueSearchVO
     * @return
     */
    Page<IssueListFieldKVVO> queryBackLogIssuesByPersonal(Long organizationId,
                                                          Long projectId,
                                                          PageRequest pageRequest,
                                                          WorkBenchIssueSearchVO workBenchIssueSearchVO);

    /**
     * 根据项目id和问题类型分页查询可以选择的父问题
     *
     * @param pageRequest
     * @param projectId
     * @param issueType
     * @param param
     * @return
     */
    Page<IssueVO> pagingQueryAvailableParents(PageRequest pageRequest, Long projectId, String issueType, String param);

    void handleUpdateComponentIssueRel(List<ComponentIssueRelVO> componentIssueRelVOList, Long projectId, Long issueId);

    void handleUpdateLabelIssue(List<LabelIssueRelVO> labelIssueRelVOList, Long issueId, Long projectId);
    
    void handleUpdateVersionIssueRel(List<VersionIssueRelVO> versionIssueRelVOList, Long projectId, Long issueId, String versionType);

    /**
     * 根据filter id获得sql
     * @param quickFilterIds
     * @return
     */
    String getQuickFilter(List<Long> quickFilterIds);

    Page<IssueLinkVO> pagedQueryByOptions(Long projectId,
                                          PageRequest pageRequest,
                                          IssueQueryVO issueQueryVO);

    IssueNumDTO queryIssueByIssueNum(Long projectId, String issueNum);

    /**
     * 分页查询我的报告
     *
     * @param organizationId
     * @param projectId
     * @param pageRequest
     * @return
     */
    Page<IssueListFieldKVVO> pagedQueryMyReported(Long organizationId,
                                                  Long projectId,
                                                  PageRequest pageRequest,
                                                  WorkBenchIssueSearchVO workBenchIssueSearchVO);

    /**
     * 工作台查询我经手的
     *
     * @param organizationId
     * @param projectId
     * @param pageRequest
     * @return
     */
    Page<IssueListFieldKVVO> pagedQueryMyAssigned(Long organizationId, Long projectId, PageRequest pageRequest, WorkBenchIssueSearchVO workBenchIssueSearchVO);

    /**
     * 处理更新tag
     *
     * @param tags
     * @param projectId
     * @param issueId
     */
    void handleUpdateTagIssueRel(List<TagVO> tags, Long projectId, Long issueId);

    /**
     * 查询issue在issueType下必填字段为空的字段
     *
     * @param projectId
     * @param organizationId
     * @param issueId
     * @param issueTypeId
     * @return
     */
    List<PageFieldViewVO> listRequiredFieldByIssueType(Long projectId,
                                                       Long organizationId,
                                                       Long issueId,
                                                       Long issueTypeId);

    List<PageFieldViewVO> listRequiredFieldByIssueTypeNoFilter(Long projectId,
                                                               Long organizationId,
                                                               Long issueId,
                                                               Long issueTypeId);

    void executionUpdateStatus(Long projectId, Long issueId, ExecutionUpdateIssueVO executionUpdateIssueVO);

    void queryUserProjects(Long organizationId, Long projectId, List<Long> projectIds, List<ProjectVO> projects, Long userId, String type);

    Page<UserDTO> pagingUserProjectUsers(PageRequest pageRequest, Long organizationId, AgileUserVO agileUserVO);

    String handleSortField(PageRequest pageRequest);

    void handlerInfluenceMap(Map<Long, List<Long>> influenceMap, Long issueId, Long statusId, Map<Long, List<IssueLinkChangeVO>> issueLinkChangeGroup, Long influenceId, InfluenceIssueVO influenceIssueVO, Boolean autoTriggered);

    void updateLinkIssueStatus(Long projectId, Long issueId, IssueDTO issueDTO, String applyType, Set<Long> influenceIssueIds);

    void statusLinkageExecutionLog(InfluenceIssueVO influenceIssueVO, Long issueId, IssueDTO influenceIssue, Boolean isSub, Map<Long, IssueStatusLinkageVO> issueStatusLinkageMap, String statusCode, String remark);

    IssueVO updateIssueWithoutRuleNotice(Long projectId,
                                         IssueUpdateVO issueUpdateVO,
                                         List<String> fieldList);

    void handleUpdateIssueWithoutRuleNotice(IssueUpdateVO issueUpdateVO,
                                            List<String> fieldList,
                                            Long projectId);

    String buildStatusLinkageContent(IssueStatusLinkageVO issueStatusLinkageVO);

    void handlerInfluenceIssue(Long projectId, String applyType, InfluenceIssueVO influenceIssueVO, Long linkIssueId,  Map<Long, IssueStatusLinkageVO> issueStatusLinkageMap, Set<Long> influenceIssueIds);

    Boolean executionUpdateInfluenceIssue(Long issueId, Long executionStatusId, IssueDTO influenceIssue, Long projectId, String applyType, InfluenceIssueVO influenceIssueVO, Boolean isSub, Map<Long, IssueStatusLinkageVO> issueStatusLinkageMap, TriggerCarrierVO triggerCarrierVO);

    void handleUpdateLabelIssueWithoutRuleNotice(List<LabelIssueRelVO> labelIssueRelVOList, Long issueId, Long projectId);

    void handleUpdateComponentIssueRelWithoutRuleNotice(List<ComponentIssueRelVO> componentIssueRelVOList, Long projectId, Long issueId);

    void handleUpdateVersionIssueRelWithoutRuleNotice(List<VersionIssueRelVO> versionIssueRelVOList, Long projectId, Long issueId, String versionType);

    IssueVO doStateMachineCustomFlowAndRuleNotice(Long projectId, Long issueId, String applyType, Set<Long> influenceIssueIds, Boolean isDemo, Long transformId, InputDTO inputDTO);

    IssueVO executionStateMachineCustomFlow(Long projectId, Long issueId, String applyType, Set<Long> influenceIssueIds);

    IssueVO doStateMachineTransformAndCustomFlow(Long projectId, Long issueId, String applyType, Set<Long> influenceIssueIds, TriggerCarrierVO triggerCarrierVO, Boolean isDemo, Long transformId, InputDTO inputDTO);

    void batchUpdateInvokeTrigger(List<TriggerCarrierVO> triggerCarriers);

    IssueVO updateIssueStatusWithoutRuleNotice(Long projectId, Long issueId, Long transformId, Long objectVersionNumber,
                                               String applyType, IssueDTO triggerIssue, boolean autoTranferFlag);

    IssueVO queryIssueCreateWithoutRuleNotice(Long projectId, Long issueId);

    IssueSubVO queryIssueSubByCreateWithoutRuleNotice(Long projectId, Long issueId);

    void batchCreateIssueInvokeTrigger(List<TriggerCarrierVO> triggerCarriers);

    void buildTriggerCarrierVO(Long projectId, Long issueId, List<TriggerCarrierVO> list, List<Long> customFieldIds);

    void addCollectionFieldIfNotNull(IssueUpdateVO issueUpdateVO, List<String> fieldList);

    Map<String, Object> processSortMap(PageRequest pageRequest,
                                       Long projectId,
                                       Long organizationId);

    void splitIssueNumProjectCodePrefix(SearchVO searchVO, Set<Long> projectIds);

    void handleUpdateParticipant(List<Long> participantIds, Long projectId, Long issueId);

    void handleUpdateParticipantWithoutRuleNotice(List<Long> participantIds, Long projectId, Long issueId);

    void handleData(Map<String, Object> reuslt,
                    ProjectVO projectVO,
                    IssueDTO issueDTO,
                    ProjectVO targetProjectVO,
                    Long projectId,
                    BatchUpdateFieldStatusVO batchUpdateFieldStatusVO);

    void setSortMap(Long organizationId, Long projectId, PageRequest pageRequest, Map<String, Object> sortMap, String mainTableAlias);

    void deleteIssueOnRequiresNew(Long projectId, Long issueId, BatchUpdateFieldStatusVO batchUpdateFieldStatusVO);

    void handleUpdateIssueProductRel(List<Long> productIds, Long projectId, Long issueId);

    List<String> listLinkContents(Long projectId, Long issueId);

    List<IssueRequiredFields> listAllRequiredField(Long projectId, Long organizationId, Long issueId, Boolean subTask);

    void copyIssueLinkContents(List<String> linkContents, Long oldIssueId, Long newIssueId, Long projectId);
}