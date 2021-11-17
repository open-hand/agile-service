package io.choerodon.agile.app.service;

import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.api.vo.business.IssueListFieldKVVO;
import io.choerodon.agile.api.vo.business.IssueVO;
import io.choerodon.core.domain.Page;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;

import java.math.BigDecimal;
import java.util.List;
import java.util.Map;

/**
 * @author zhaotianxin
 * @date 2021-10-15 13:35
 */
public interface WorkHoursService {
    /**
     *  查询工时日志
     * @param organizationId
     * @param projectIds
     * @param pageRequest
     * @param workHoursSearchVO
     * @return
     */
    Page<WorkHoursLogVO> pageWorkHoursLogByProjectIds(Long organizationId, List<Long> projectIds, PageRequest pageRequest, WorkHoursSearchVO workHoursSearchVO);

    /**
     *
     * @param organizationId
     * @param pageRequest
     * @param workHoursSearchVO
     * @return
     */
    Page<WorkHoursLogVO> pageWorkHoursLogByOrgId(Long organizationId, PageRequest pageRequest, WorkHoursSearchVO workHoursSearchVO);

    /**
     * 工时日历
     * @param organizationId
     * @param projectIds
     * @param pageRequest
     * @param workHoursSearchVO
     * @return
     */
    Page<WorkHoursCalendarVO> workHoursCalendar(Long organizationId, List<Long> projectIds, PageRequest pageRequest, WorkHoursSearchVO workHoursSearchVO, Boolean isOrg);

    /**
     * 组织层：工时日历
     * @param organizationId
     * @param pageRequest
     * @param workHoursSearchVO
     * @return
     */
    Page<WorkHoursCalendarVO> workHoursCalendarByOrg(Long organizationId, PageRequest pageRequest, WorkHoursSearchVO workHoursSearchVO);

    /**
     * 工时日历查用户的登记详情
     * @param organizationId
     * @param projectIds
     * @param workHoursSearchVO
     * @return
     */
    Map<String, List<WorkHoursLogVO>> workHoursCalendarInfoByUserId(Long organizationId, List<Long> projectIds, Long userId, WorkHoursSearchVO workHoursSearchVO);

    /**
     * 工时日历查用户的登记详情
     * @param organizationId
     * @param userId
     * @param workHoursSearchVO
     * @return
      */
    Map<String, List<WorkHoursLogVO>> workHoursCalendarOrgInfoByUserId(Long organizationId, Long userId, WorkHoursSearchVO workHoursSearchVO);

    /**
     * 查询项目下的问题
     * @param projectId
     * @param pageRequest
     * @param params
     * @return
     */
    Page<IssueVO> queryIssue(Long projectId, PageRequest pageRequest, String params);

    /**
     * 项目层查询工时统计
     * @param organizationId
     * @param projectIds
     * @param workHoursSearchVO
     * @return
     */
    Map<String, BigDecimal> countWorkHours(Long organizationId, List<Long> projectIds, WorkHoursSearchVO workHoursSearchVO);


    /**
     * 项目层查询工时统计
     * @param organizationId
     * @param workHoursSearchVO
     * @return
     */
    Map<String, BigDecimal> countWorkHoursOnOrganizationLevel(Long organizationId, WorkHoursSearchVO workHoursSearchVO);

    Map<Long, WorkHoursCountVO> countWorkHoursCalendar(Long organizationId, List<Long> projectIds, WorkHoursSearchVO workHoursSearchVO);

    Page<IssueListFieldKVVO> pageQueryIssues(Long organizationId, List<Long> projectIds, PageRequest pageRequest, Boolean containsSubIssue, SearchVO searchVO);

    Page<IssueWorkHoursVO> pageQueryAssignee(Long organizationId, List<Long> projectIds, PageRequest pageRequest, SearchVO searchVO);

    Page<IssueWorkHoursVO> pageQueryProject(Long organizationId, PageRequest pageRequest, SearchVO searchVO);

    Page<IssueWorkHoursVO> pageQueryAssigneeOnOrganizationLevel(Long organizationId, PageRequest pageRequest, SearchVO searchVO);

    Page<IssueListFieldKVVO> pageQueryIssuesOnOrganizationLevel(Long organizationId, PageRequest pageRequest, Boolean containsSubIssue, SearchVO searchVO);

    BigDecimal countIssueWorkHours(Long organizationId, List<Long> projetcIds, SearchVO searchVO);

    BigDecimal countIssueWorkHoursOnOrganizationLevel(Long organizationId, SearchVO searchVO);
}
