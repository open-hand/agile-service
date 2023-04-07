package io.choerodon.agile.app.service;

import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Set;

import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.api.vo.business.IssueCreateVO;
import io.choerodon.agile.api.vo.business.IssueUpdateVO;
import io.choerodon.agile.api.vo.business.IssueVO;
import io.choerodon.agile.api.vo.business.SystemFieldOverrideConfigVO;
import io.choerodon.agile.api.vo.event.ProjectEvent;
import io.choerodon.agile.api.vo.search.Condition;
import io.choerodon.agile.api.vo.waterfall.PredecessorIssueStatusLinkageVO;
import io.choerodon.agile.api.vo.waterfall.WfDeliverableVO;
import io.choerodon.agile.infra.dto.StatusDTO;
import io.choerodon.agile.infra.dto.StatusFieldValueSettingDTO;
import io.choerodon.agile.infra.dto.business.IssueConvertDTO;
import io.choerodon.agile.infra.dto.business.IssueDTO;
import io.choerodon.agile.infra.dto.business.IssueDetailDTO;

import org.hzero.core.util.Pair;

/**
 * @author superlee
 * @since 2022-02-15
 */
public interface AgileWaterfallService {

    List<String> getSystemFieldContext(String code);

    Map<String, List<SystemFieldOverrideConfigVO>> querySystemFieldOverrideConfig();

    void initProject(ProjectEvent projectEvent, Set<String> codes);

    List<IssueTypeVO> filterWaterfallIssueTypes(List<IssueTypeVO> issueTypes, List<Long> issueTypeIds);

    Long initWaterfallStateMachine(Long organizationId, ProjectEvent projectEvent);

    void checkBeforeCreateIssue(IssueCreateVO issueCreateVO, String applyType);

    void handlerWaterfallAfterCreateIssue(Long projectId, Long issueId, IssueCreateVO issueCreateVO);

    void waterfallIssueDetailDTOToVO(IssueVO issueVO, Map<Long, IssueTypeVO> issueTypeDTOMap, Map<Long, StatusVO> statusMapDTOMap, Map<Long, PriorityVO> priorityDTOMap);

    void checkBeforeUpdateIssue(IssueUpdateVO issueUpdateVO, Long projectId, List<String> fieldList);

    void buildWaterfallFieldList(List<String> fieldList, IssueUpdateVO issueUpdateVO);

    void handleUpdateWaterfallField(Long projectId, IssueUpdateVO issueUpdateVO);

    void handleUpdateWaterfallFieldWithoutRuleNotice(Long projectId, IssueUpdateVO issueUpdateVO);

    void checkUpdateIssueTypeCode(Long projectId, IssueConvertDTO issueConvertDTO, IssueUpdateTypeVO issueUpdateTypeVO, List<String> fieldList);

    List<String> queryFieldOrderByIssueType(String issueType);

    Map<String, PageConfigFieldEditedVO> fieldEdited(String issueType);

    /**
     * 创建交付物
     *
     * @param issueId          issueId
     * @param wfDeliverableVOS wfDeliverableVOS
     */
    void createDeliverableService(Long issueId, List<WfDeliverableVO> wfDeliverableVOS);

    void handlerTransferSubTask(IssueConvertDTO issueConvertDTO, Long projectId, List<String> fieldList);

    void handlerCopyIssue(IssueDetailDTO issueDetailDTO, Long newIssueId, Long projectId);

    void handlerAfterCreateSubIssue(Long projectId, Long issueId, IssueSubCreateVO issueSubCreateVO);

    void deleteIssueForWaterfall(Long projectId, Long issueId, IssueConvertDTO issueConvertDTO);

    void handlerSubIssueUpdateParent(Long projectId, Long issueId, Long parentIssueId);

    void handlerWaterfallUpdateIssue(String issueType, List<String> fieldList, Long projectId, IssueUpdateVO issueUpdateVO, IssueDTO originIssue);

    void handlerUpdateIssueTypeCode(Long projectId, String originType, IssueUpdateTypeVO issueUpdateTypeVO);

    void handlerDeleteSprint(Long projectId, Long sprintId);

    void handleUpdateEstimatedTime(Long projectId, Long issueId, Date estimatedStartTime, Date estimatedEndTime);

    void appendWaterfallFiledSql(StringBuilder sqlQuery, QuickFilterValueVO quickFilterValueVO, String value, String operation, Long projectId);

    List<StatusDTO> queryWaterfallNotAllowedTransferStatus(IssueDTO issueDTO);

    void handlerWaterfallFieldValue(String fieldCode, IssueUpdateVO issueUpdateVO, Map<String, Object> specifyMap, List<StatusFieldValueSettingDTO> statusFieldValueSettings, IssueDTO issueDTO, List<String> field);

    boolean updateWaterfallParentStatus(IssueDTO issueDTO, Set<Long> influenceIssueIds, String applyType);

    void updatePredecessorIssueStatus(Long projectId, Long issueId, IssueDTO issueDTO, String applyType, Set<Long> influenceIssueIds);

    Map<Long, List<PredecessorIssueStatusLinkageVO>> listPredecessorIssueMapByIssueTypeAndStatusIds(Long projectId, Long organizationId, Long issueTypeId, List<Long> statusIds);

    List<FieldTableVO> getWaterfallField();

    Map<String, Set<Long>> queryPredecessorIssues(Long projectId, Long issueId, Set<String> predecessorTypes);

    Map<Long, Integer> getIssueProgressMap(List<Long> projectIds, List<Long> issueIds);

    void getWaterfallFieldCodes(List<String> fieldCodes, Long issueTypeId);

    void handleMilestoneProgressByStatusId(Long projectId, Long issueId, Long statusId, String typeCode);

    List<Long> selectDescendants(Long projectId, Long issueId);

    void deleteByWorkSpaceId(Long projectId, Long workSpaceId);

    String parseWaterfallSql(FieldTableVO fieldTable,
                             Condition condition,
                             Set<Long> projectIds,
                             List<? extends Object> values,
                             Pair<String, String> dataPair,
                             boolean isSelector);

    void copyPredecessorIssueStatusLinkage(List<PredecessorIssueStatusLinkageVO> predecessorIssueStatusLinkages,
                                           Long projectId,
                                           Long issueTypeId,
                                           Long statusId);

    /**
     * 判断瀑布项目的所有后代是否全部已完成
     *
     * @param issueDTO
     * @param projectId
     * @return
     */
    boolean validateAllSubIssueUnCompleted(IssueDTO issueDTO, Long projectId);

    /**
     * 处理需求池关联的瀑布工作项
     * @param backlogId 需求池Id
     * @param issueVOS 工作项列表
     * @return resultList
     */
    List<IssueVO> handleParentId(Long backlogId, List<IssueVO> issueVOS);
}
