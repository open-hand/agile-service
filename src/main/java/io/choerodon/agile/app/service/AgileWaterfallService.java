package io.choerodon.agile.app.service;

import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.api.vo.business.IssueCreateVO;
import io.choerodon.agile.api.vo.business.IssueUpdateVO;
import io.choerodon.agile.api.vo.business.IssueVO;
import io.choerodon.agile.api.vo.business.SystemFieldOverrideConfigVO;
import io.choerodon.agile.api.vo.event.ProjectEvent;
import io.choerodon.agile.api.vo.waterfall.WfDeliverableVO;
import io.choerodon.agile.infra.dto.business.IssueConvertDTO;
import io.choerodon.agile.infra.dto.business.IssueDTO;
import io.choerodon.agile.infra.dto.business.IssueDetailDTO;

import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Set;

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

    void checkUpdateIssueTypeCode(Long projectId, IssueConvertDTO issueConvertDTO, IssueUpdateTypeVO issueUpdateTypeVO);

    List<String> queryFieldOrderByIssueType(String issueType);

    Map<String, PageConfigFieldEditedVO> fieldEdited(String issueType);

    /**
     * 创建交付物
     * @param issueId
     * @param wfDeliverableVOS
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
}
