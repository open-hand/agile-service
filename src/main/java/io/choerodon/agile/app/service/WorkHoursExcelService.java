package io.choerodon.agile.app.service;

import io.choerodon.agile.api.vo.SearchVO;
import io.choerodon.agile.api.vo.WorkHoursSearchVO;
import org.springframework.web.context.request.ServletRequestAttributes;

import javax.servlet.http.HttpServletResponse;
import java.util.List;

/**
 * @author zhaotianxin
 * @date 2021-10-19 14:11
 */
public interface WorkHoursExcelService {
    /**
     * 导出工时日志
     * @param organizationId
     * @param projectIds
     * @param workHoursSearchVO
     * @param requestAttributes
     * @param isOrg
     */
    void exportWorkHoursLog(Long organizationId, List<Long> projectIds, WorkHoursSearchVO workHoursSearchVO, ServletRequestAttributes requestAttributes, Boolean isOrg);

    /**
     * 项目层导出工时日志
     * @param organizationId
     * @param projectIds
     * @param workHoursSearchVO
     * @param requestAttributes
     */
    void exportWorkHoursLogOnProjectLevel(Long organizationId, List<Long> projectIds, WorkHoursSearchVO workHoursSearchVO, ServletRequestAttributes requestAttributes);

    /**
     * 组织层导出工时日志
     * @param organizationId
     * @param workHoursSearchVO
     * @param requestAttributes
     */
    void exportWorkHoursLogOnOrganizationLevel(Long organizationId, WorkHoursSearchVO workHoursSearchVO, ServletRequestAttributes requestAttributes);

    /**
     * 导出工时日历
     * @param organizationId
     * @param projectIds
     * @param workHoursSearchVO
     * @param requestAttributes
     * @param isOrg
     */
    void exportWorkHoursCalendar(Long organizationId, List<Long> projectIds, WorkHoursSearchVO workHoursSearchVO, ServletRequestAttributes requestAttributes, Boolean isOrg);

    /**
     * 组织层导出工时日历
     * @param organizationId
     * @param workHoursSearchVO
     * @param requestAttributes
     * @param isOrg
     */
    void exportWorkHoursCalendarOnOrganizationLevel(Long organizationId, WorkHoursSearchVO workHoursSearchVO, ServletRequestAttributes requestAttributes, Boolean isOrg);

    /**
     * 项目层导出工时日历
     * @param organizationId
     * @param projectId
     * @param workHoursSearchVO
     * @param requestAttributes
     * @param isOrg
     */
    void exportWorkHoursCalendarOnProjectLevel(Long organizationId, Long projectId, WorkHoursSearchVO workHoursSearchVO, ServletRequestAttributes requestAttributes, Boolean isOrg);

    void exportIssueWorkHours(Long organizationId, List<Long> projectIds, ServletRequestAttributes currentRequestAttributes, boolean isOrg, SearchVO searchVO, boolean containsSubIssue);

    void exportIssueWorkHoursOnOrganizationLevel(Long organizationId, ServletRequestAttributes currentRequestAttributes, boolean isOrg, SearchVO searchVO,  Boolean containsSubIssue);

    void exportIssueWorkHoursOnProjectLevel(Long organizationId, Long projectId, ServletRequestAttributes currentRequestAttributes, boolean isOrg, SearchVO searchVO, Boolean containsSubIssue);
}
