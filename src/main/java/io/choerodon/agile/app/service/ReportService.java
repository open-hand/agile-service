package io.choerodon.agile.app.service;

import java.util.Date;
import java.util.List;
import java.util.Map;

import com.alibaba.fastjson.JSONObject;

import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.api.vo.business.IssueListVO;
import io.choerodon.agile.api.vo.report.CustomChartDataVO;
import io.choerodon.agile.api.vo.report.CustomChartSearchVO;
import io.choerodon.agile.infra.dto.GroupDataChartDTO;
import io.choerodon.agile.infra.dto.business.GroupDataChartListDTO;
import io.choerodon.agile.infra.mapper.ReportMapper;
import io.choerodon.core.domain.Page;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;

/**
 * @author dinghuang123@gmail.com
 * @since 2018/6/19
 */
public interface ReportService {

    /**
     * 燃尽图报告信息
     *
     * @param projectId   projectId
     * @param sprintId    sprintId
     * @param burnDownSearchVO burnDownSearchVO
     * @return ReportIssueVO
     */
    List<ReportIssueVO> queryBurnDownReport(Long projectId, Long sprintId, BurnDownSearchVO burnDownSearchVO);

    /**
     * 查看累积流量图
     *
     * @param projectId               projectId
     * @param cumulativeFlowFilterVO cumulativeFlowFilterVO
     * @return CumulativeFlowDiagramVO
     */
    List<CumulativeFlowDiagramVO> queryCumulativeFlowDiagram(Long projectId, CumulativeFlowFilterVO cumulativeFlowFilterVO);

    Page<IssueListVO> queryIssueByOptions(Long projectId, Long versionId, String status, String type, PageRequest pageRequest, Long organizationId);

    Map<String, Object> queryVersionLineChart(Long projectId, Long versionId, String type);

    List<VelocitySprintVO> queryVelocityChart(Long projectId, String type);

    /**
     * 根据项目id和字段名称查询饼图
     *
     * @param projectId      projectId
     * @param fieldName      fieldName
     * @param organizationId organizationId
     * @param startDate      startDate
     * @param endDate        endDate
     * @param sprintId       sprintId
     * @param versionId      versionId
     * @param statusId       statusId
     * @param customFieldId  customFieldId
     * @return PieChartVO
     */
    List<PieChartVO> queryPieChart(Long projectId, String fieldName, Long organizationId, Date startDate, Date endDate, Long sprintId, Long versionId, Long statusId, Long customFieldId);

    List<GroupDataChartDTO> queryEpicChart(Long projectId, Long epicId, String type);

    List<GroupDataChartDTO> queryVersionChart(Long projectId, Long versionId, String type);

    List<GroupDataChartListDTO> queryEpicChartList(Long projectId, Long epicId, Long organizationId);

    List<GroupDataChartListDTO> queryVersionChartList(Long projectId, Long versionId, Long organizationId);

    /**
     * 查询燃尽图坐标信息
     *
     * @param projectId projectId
     * @param sprintId  sprintId
     * @param burnDownSearchVO burnDownSearchVO
     * @return Coordinate
     */
    JSONObject queryBurnDownCoordinate(Long projectId, Long sprintId, BurnDownSearchVO burnDownSearchVO);

    /**
     * 查询epic和版本燃耗图坐标信息
     *
     * @param projectId projectId
     * @param id        id
     * @param type      type
     * @return BurnDownReportCoordinateVO
     */
    List<BurnDownReportCoordinateVO> queryBurnDownCoordinateByType(Long projectId, Long id, String type);

    /**
     * 查询epic和版本燃尽图报告信息
     *
     * @param projectId projectId
     * @param id        id
     * @param type      type
     * @return BurnDownReportVO
     */
    BurnDownReportVO queryBurnDownReportByType(Long projectId, Long id, String type, Long organizationId);

    void setReportMapper(ReportMapper reportMapper);

    /**
     * 查询问题类型分布图
     *
     * @param projectId projectId
     * @return IssueTypeDistributionChartVO
     */
    List<IssueTypeDistributionChartVO> queryIssueTypeDistributionChart(Long projectId);

    /**
     * 版本进度图,排序前5个版本
     *
     * @param projectId projectId
     * @return IssueTypeDistributionChartVO
     */
    List<IssueTypeDistributionChartVO> queryVersionProgressChart(Long projectId);

    /**
     * 查询问题优先级分布图
     *
     * @param projectId projectId
     * @return IssuePriorityDistributionChartVO
     */
    List<IssuePriorityDistributionChartVO> queryIssuePriorityDistributionChart(Long projectId, Long organizationId);

    /**
     * 修复累积流图
     */
    void fixCumulativeFlowDiagram();

    /**
     * 插件迭代bug统计趋势
     * @param projectId 项目id
     * @param sprintId 冲刺id
     * @return 趋势
     */
    IssueCountVO selectBugBysprint(Long projectId, Long sprintId);

    /**
     * 查询自定义报表
     * @param customChartSearchVO 查询参数
     * @param projectId 项目id
     * @param organizationId 组织id
     * @return 自定义报表数据
     */
    CustomChartDataVO queryCustomChartData(CustomChartSearchVO customChartSearchVO, Long projectId, Long organizationId);
}
