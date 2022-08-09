package io.choerodon.agile.app.service;

import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.api.vo.business.IssueListVO;
import io.choerodon.agile.api.vo.business.IssueSearchVO;
import io.choerodon.agile.api.vo.business.SprintDetailVO;
import io.choerodon.agile.infra.dto.IssueSprintDTO;
import io.choerodon.agile.infra.dto.business.SprintConvertDTO;
import io.choerodon.agile.infra.dto.SprintDTO;
import io.choerodon.core.domain.Page;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;

import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Created by jian_zhang02@163.com on 2018/5/15.
 */
public interface SprintService {

    SprintDetailVO createSprint(Long projectId);

    SprintDetailVO createSprintByDetails(Long projectId, SprintCreateVO sprintCreateVO);

    SprintDetailVO updateSprint(Long projectId, SprintUpdateVO sprintUpdateVO);

    Boolean deleteSprint(Long projectId, Long sprintId);

    Map<String, Object> queryByProjectId(Long projectId, Map<String, Object> searchParamMap, List<Long> quickFilterIds, Long organizationId, List<Long> assigneeFilterIds);

    List<SprintNameVO> queryNameByOptions(Long projectId, List<String> sprintStatusCodes);

    SprintDetailVO startSprint(Long projectId, SprintUpdateVO sprintUpdateVO, boolean noticeIam);

    Boolean completeSprint(Long projectId, SprintCompleteVO sprintCompleteVO);

    SprintCompleteMessageVO queryCompleteMessageBySprintId(Long projectId, Long sprintId);

    SprintDTO getActiveSprint(Long projectId);

    SprintDetailVO querySprintById(Long projectId, Long sprintId);

    Page<IssueListVO> queryIssueByOptions(Long projectId, Long sprintId, String status, PageRequest pageRequest, Long organizationId);

    String getQuickFilter(List<Long> quickFilterIds);

    String queryCurrentSprintCreateName(Long projectId);

    List<SprintUnClosedVO> queryUnClosedSprint(Long projectId);

    ActiveSprintVO queryActiveSprint(Long projectId, Long organizationId);

    /**
     * 查询冲刺期间非工作日
     *
     * @param projectId      projectId
     * @param sprintId       sprintId
     * @param organizationId organizationId
     * @return Date
     */
    List<String> queryNonWorkdays(Long projectId, Long sprintId, Long organizationId);

    Boolean checkName(Long projectId, String sprinName);

    SprintConvertDTO create(SprintConvertDTO sprintConvertDTO);

    SprintConvertDTO update(SprintConvertDTO sprintConvertDTO);

    Boolean delete(SprintConvertDTO sprintConvertDTO);

    List<SprintSearchVO> unCloseSprint(Long projectId, Map<String, Object> searchParamMap);

    Page<IssueSearchVO> issuePageBySprint(Long projectId, Long sprintId, PageRequest pageRequest, Map<String, Object> searchParamMap);

    Page<IssueSearchVO> todoIssuePage(Long projectId, PageRequest pageRequest, Map<String, Object> searchParamMap);

    /**
     * 开启冲刺前提示未预估故事点的故事和与未预估工时的问题个数
     * @param projectId
     * @param sprintId
     * @return
     */
    SprintStartMessageVO selectSprintStartMessage(Long projectId, Long sprintId);

    /**
     * 拆分agile_issue_sprint_rel join agile_sprint查询慢的问题
     *
     * @param projectIds
     * @param issueIds
     * @return
     */
    Map<Long, List<IssueSprintDTO>> queryIssueSprintMap(Set<Long> projectIds, Set<Long> issueIds);
}
