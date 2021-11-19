package io.choerodon.agile.app.service.impl;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.api.vo.business.ExportIssuesVO;
import io.choerodon.agile.app.service.*;
import io.choerodon.agile.infra.dto.*;
import io.choerodon.agile.infra.dto.business.IssueDTO;
import io.choerodon.agile.infra.enums.IssueConstant;
import io.choerodon.agile.infra.feign.BaseFeignClient;
import io.choerodon.agile.infra.feign.vo.OrganizationInfoVO;
import io.choerodon.agile.infra.mapper.FileOperationHistoryMapper;
import io.choerodon.agile.infra.mapper.ObjectSchemeFieldMapper;
import io.choerodon.agile.infra.utils.ConvertUtil;
import io.choerodon.agile.infra.utils.ExcelUtil;
import io.choerodon.agile.infra.utils.MultipartExcel;
import io.choerodon.agile.infra.utils.PageUtil;
import io.choerodon.core.client.MessageClientC7n;
import io.choerodon.core.domain.Page;
import io.choerodon.core.exception.CommonException;
import io.choerodon.core.oauth.DetailsHelper;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;
import io.choerodon.mybatis.pagehelper.domain.Sort;
import org.apache.commons.lang3.time.DateUtils;
import org.apache.poi.hssf.util.HSSFColor;
import org.apache.poi.ss.usermodel.*;
import org.apache.poi.ss.util.CellRangeAddress;
import org.apache.poi.xssf.streaming.SXSSFCell;
import org.apache.poi.xssf.streaming.SXSSFRow;
import org.apache.poi.xssf.streaming.SXSSFSheet;
import org.apache.poi.xssf.streaming.SXSSFWorkbook;
import org.hzero.boot.file.FileClient;
import org.hzero.core.base.BaseConstants;
import org.modelmapper.ModelMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;
import org.springframework.web.multipart.MultipartFile;

import javax.servlet.http.HttpServletResponse;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.text.DateFormat;
import java.text.NumberFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * @author zhaotianxin
 * @date 2021-10-19 14:24
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class WorkHoursExcelServiceImpl implements WorkHoursExcelService {
    protected static final Logger LOGGER = LoggerFactory.getLogger(WorkHoursExcelServiceImpl.class);
    private static final String EXPORT_WORK_HOURS_LOG = "agile-export-work-hours-log";
    private static final String EXPORT_WORK_HOURS_CALENDAR = "agile-export-work-hours-calendar";
    private static final String EXPORT_ISSUE_WORK_HOURS = "agile-export-issue-work-hours";
    private static final String EXCELCONTENTTYPE = "application/vnd.ms-excel";
    private static final String SUCCESS = "success";
    private static final String FAILED = "failed";
    private static final String DOING = "doing";
    private static final String DOWNLOAD_FILE = "download_file_work_hours_log";
    private static final String DOWNLOAD_CALENDAR_FILE = "download_file_work_hours_calendar";
    private static final String DOWNLOAD_ISSUE_WORK_FILE = "download_file_issue_work_hours";
    private static final List<ExcelTitleVO> WORK_HOURS_LOG_LIST = new ArrayList<>();
    private static final List<ExcelTitleVO> WORK_HOURS_CALENDAR_LIST = new ArrayList<>();
    private static final List<ExcelTitleVO> WORK_HOURS_CALENDAR_REPORT_LIST = new ArrayList<>();
    private static final List<ExcelTitleVO> ISSUE_WORK_HOURS_REPORT_LIST = new ArrayList<>();
    private static final String EXPORT_ERROR_WORKBOOK_CLOSE = "error.close.workbook";

    @Autowired
    private WorkHoursService workHoursService;

    @Autowired
    private BaseFeignClient baseFeignClient;

    @Autowired
    private FileOperationHistoryMapper fileOperationHistoryMapper;

    @Autowired
    private FileClient fileClient;

    @Autowired
    private ObjectMapper objectMapper;

    @Autowired
    private MessageClientC7n messageClientC7n;
    @Autowired
    private WorkGroupService workGroupService;
    @Autowired
    private ExcelService excelService;
    @Autowired
    private ObjectSchemeFieldMapper objectSchemeFieldMapper;
    @Autowired
    private ModelMapper modelMapper;
    @Autowired(required = false)
    private AgilePluginService agilePluginService;

    static {
        WORK_HOURS_LOG_LIST.add(new ExcelTitleVO("登记人", "userName", 4000));
        WORK_HOURS_LOG_LIST.add(new ExcelTitleVO("耗费时间（单位：小时）", "workTime", 6000));
        WORK_HOURS_LOG_LIST.add(new ExcelTitleVO("工作日期", "workDate", 4000));
        WORK_HOURS_LOG_LIST.add(new ExcelTitleVO("工作项类型", "issueTypeName", 4000));
        WORK_HOURS_LOG_LIST.add(new ExcelTitleVO("工作项编号", "issueNum", 4000));
        WORK_HOURS_LOG_LIST.add(new ExcelTitleVO("工作项概要", "summary", 10000));
        WORK_HOURS_LOG_LIST.add(new ExcelTitleVO("状态", "statusName", 4000));
        WORK_HOURS_LOG_LIST.add(new ExcelTitleVO("所属项目", "projectName", 4000));
        WORK_HOURS_CALENDAR_LIST.add(new ExcelTitleVO("成员", "userName", 6000));
        WORK_HOURS_CALENDAR_LIST.add(new ExcelTitleVO("总计登记工时（单位：小时）", "allEstimateTime", 4000));
        WORK_HOURS_CALENDAR_REPORT_LIST.add(new ExcelTitleVO("工作组", "workGroupName", 12000));
        WORK_HOURS_CALENDAR_REPORT_LIST.add(new ExcelTitleVO("成员数量", "userCount", 4000));
        WORK_HOURS_CALENDAR_REPORT_LIST.add(new ExcelTitleVO("实际登记工时成员数量", "actualUserCount", 8000));
        WORK_HOURS_CALENDAR_REPORT_LIST.add(new ExcelTitleVO("实际登记工时成员占比", "actualUserProportion", 8000));
        WORK_HOURS_CALENDAR_REPORT_LIST.add(new ExcelTitleVO("工时登记不饱和人数", "unsaturatedUserCount", 8000));
        WORK_HOURS_CALENDAR_REPORT_LIST.add(new ExcelTitleVO("不饱和人数占比", "unsaturatedUserProportion", 8000));
        WORK_HOURS_CALENDAR_REPORT_LIST.add(new ExcelTitleVO("工时登记不饱和人天（次数）", "unsaturatedTimes", 8000));

        ISSUE_WORK_HOURS_REPORT_LIST.add(new ExcelTitleVO("工作项类型", "typeName",4000));
        ISSUE_WORK_HOURS_REPORT_LIST.add(new ExcelTitleVO("概要", "summary",12000));
        ISSUE_WORK_HOURS_REPORT_LIST.add(new ExcelTitleVO(IssueConstant.ISSUE_CN + "编号", "issueNum",4000));
        ISSUE_WORK_HOURS_REPORT_LIST.add(new ExcelTitleVO("经办人", "assigneeName",4000));
        ISSUE_WORK_HOURS_REPORT_LIST.add(new ExcelTitleVO("状态", "statusName",4000));
        ISSUE_WORK_HOURS_REPORT_LIST.add(new ExcelTitleVO("工时", "workTime",4000));
        ISSUE_WORK_HOURS_REPORT_LIST.add(new ExcelTitleVO("历史累计工时", "cumulativeWorkTime",4000));
        ISSUE_WORK_HOURS_REPORT_LIST.add(new ExcelTitleVO("预估总工时", "estimateTime",4000));
        ISSUE_WORK_HOURS_REPORT_LIST.add(new ExcelTitleVO("偏差率", "deviationRate",4000));
        ISSUE_WORK_HOURS_REPORT_LIST.add(new ExcelTitleVO("描述", "description",4000));
        ISSUE_WORK_HOURS_REPORT_LIST.add(new ExcelTitleVO("优先级", "priorityName",4000));
        ISSUE_WORK_HOURS_REPORT_LIST.add(new ExcelTitleVO("冲刺", "sprintName",4000));
        ISSUE_WORK_HOURS_REPORT_LIST.add(new ExcelTitleVO("报告人", "reporterName",4000));
        ISSUE_WORK_HOURS_REPORT_LIST.add(new ExcelTitleVO("故事点", "storyPoints",4000));
        ISSUE_WORK_HOURS_REPORT_LIST.add(new ExcelTitleVO("剩余预估时间", "remainingTime",4000));
        ISSUE_WORK_HOURS_REPORT_LIST.add(new ExcelTitleVO("版本", "versionName",4000));
        ISSUE_WORK_HOURS_REPORT_LIST.add(new ExcelTitleVO("修复的版本", "fixVersionName",4000));
        ISSUE_WORK_HOURS_REPORT_LIST.add(new ExcelTitleVO("影响的版本", "influenceVersionName",4000));
        ISSUE_WORK_HOURS_REPORT_LIST.add(new ExcelTitleVO("所属史诗", "epicName",4000));
        ISSUE_WORK_HOURS_REPORT_LIST.add(new ExcelTitleVO("标签", "labelName",4000));
        ISSUE_WORK_HOURS_REPORT_LIST.add(new ExcelTitleVO("模块", "componentName",4000));
        ISSUE_WORK_HOURS_REPORT_LIST.add(new ExcelTitleVO("创建时间", "creationDate",4000));
        ISSUE_WORK_HOURS_REPORT_LIST.add(new ExcelTitleVO("最后更新时间", "lastUpdateDate",4000));
        ISSUE_WORK_HOURS_REPORT_LIST.add(new ExcelTitleVO("预计开始时间", "estimatedStartTime",4000));
        ISSUE_WORK_HOURS_REPORT_LIST.add(new ExcelTitleVO("预计结束时间", "estimatedEndTime",4000));
        ISSUE_WORK_HOURS_REPORT_LIST.add(new ExcelTitleVO("实际开始时间", "actualStartTime",4000));
        ISSUE_WORK_HOURS_REPORT_LIST.add(new ExcelTitleVO("实际结束时间", "actualEndTime",4000));
        ISSUE_WORK_HOURS_REPORT_LIST.add(new ExcelTitleVO("创建人", "createdUserName",4000));
        ISSUE_WORK_HOURS_REPORT_LIST.add(new ExcelTitleVO("更新人", "lastUpdatedUserName",4000));
        ISSUE_WORK_HOURS_REPORT_LIST.add(new ExcelTitleVO("主要负责人", "mainResponsibleName",4000));
        ISSUE_WORK_HOURS_REPORT_LIST.add(new ExcelTitleVO("环境", "environmentName",4000));
        ISSUE_WORK_HOURS_REPORT_LIST.add(new ExcelTitleVO("已耗费时间", "spentWorkTime",4000));
        ISSUE_WORK_HOURS_REPORT_LIST.add(new ExcelTitleVO("当前预估时间", "allEstimateTime",4000));
        ISSUE_WORK_HOURS_REPORT_LIST.add(new ExcelTitleVO("Tag", "tags",4000));
        ISSUE_WORK_HOURS_REPORT_LIST.add(new ExcelTitleVO("关联" + IssueConstant.ISSUE_CN, "relatedIssue",4000));
        ISSUE_WORK_HOURS_REPORT_LIST.add(new ExcelTitleVO("史诗名称", "epicSelfName",4000));
        ISSUE_WORK_HOURS_REPORT_LIST.add(new ExcelTitleVO("参与人" , "participant",4000));
    }

    @Override
    public void exportWorkHoursLog(Long organizationId,
                                   List<Long> projectIds,
                                   WorkHoursSearchVO workHoursSearchVO,
                                   ServletRequestAttributes requestAttributes,
                                   Boolean isOrg) {
        RequestContextHolder.setRequestAttributes(requestAttributes);
        Long userId = DetailsHelper.getUserDetails().getUserId();
        String websocketKey = buildWebSocketKey(EXPORT_WORK_HOURS_LOG, isOrg, organizationId, projectIds.get(0));
        String excelName = buildExcelName(organizationId, "工时日志", projectIds.get(0), isOrg);
        // 发送websocket
        Long projectId = 0L;
        if (Boolean.FALSE.equals(isOrg)) {
            projectId = projectIds.get(0);
        }
        FileOperationHistoryDTO fileOperationHistoryDTO = initFileOperationHistory(projectId, organizationId, userId, DOING, DOWNLOAD_FILE, websocketKey);
        // 构建excel标题栏
        Workbook workbook = ExcelUtil.initWorkHoursExportWorkbook("工时日志", WORK_HOURS_LOG_LIST);
        // 查询数据并写入excel
        ExcelCursorDTO cursor = new ExcelCursorDTO(1, 0, 1000);
        Sort.Order order = new Sort.Order(Sort.Direction.DESC, "creationDate");
        Sort sort = new Sort(order);
        sendProcess(fileOperationHistoryDTO, userId, 0.0, websocketKey);
        double lastProcess = 0D;
        while (true) {
            PageRequest pageRequest = new PageRequest(cursor.getPage(), cursor.getSize());
            pageRequest.setSort(sort);
            Page<WorkHoursLogVO> page = workHoursService.pageWorkHoursLogByProjectIds(organizationId, projectIds, pageRequest, workHoursSearchVO);
            if (CollectionUtils.isEmpty(page.getContent())) {
                break;
            }
            List<WorkHoursLogVO> content = page.getContent();
            List<WorkHoursExportVO> workHoursExportVOS = new ArrayList<>();
            content.forEach(v -> buildExportVO(v, workHoursExportVOS));
            ExcelUtil.writeWorkHoursLog(workbook, "工时日志", WorkHoursExportVO.class, workHoursExportVOS, WORK_HOURS_LOG_LIST, cursor);
            boolean hasNextPage = (cursor.getPage() + 1) < page.getTotalPages();
            cursor.clean();
            double process = getProcess(cursor.getPage(), page.getTotalPages());
            if (process - lastProcess >= 0.1) {
                sendProcess(fileOperationHistoryDTO, userId, process, websocketKey);
                lastProcess = process;
            }
            if (!hasNextPage) {
                break;
            }
        }
        // 上传excel到minio并发送websocket
        downloadExcel(workbook, excelName, organizationId, websocketKey, userId, fileOperationHistoryDTO);
    }

    @Override
    public void exportWorkHoursCalendar(Long organizationId,
                                        List<Long> projectIds,
                                        WorkHoursSearchVO workHoursSearchVO,
                                        ServletRequestAttributes requestAttributes,
                                        Boolean isOrg) {
        RequestContextHolder.setRequestAttributes(requestAttributes);
        Long userId = DetailsHelper.getUserDetails().getUserId();
        String websocketKey = buildWebSocketKey(EXPORT_WORK_HOURS_CALENDAR, isOrg, organizationId, projectIds.get(0));
        String excelName = buildExcelName(organizationId, "工时日历", projectIds.get(0), isOrg);
        // 发送websocket
        Long projectId = 0L;
        if (Boolean.FALSE.equals(isOrg)) {
            projectId = projectIds.get(0);
        }
        FileOperationHistoryDTO fileOperationHistoryDTO = initFileOperationHistory(projectId, organizationId, userId, DOING, DOWNLOAD_CALENDAR_FILE, websocketKey);
        sendProcess(fileOperationHistoryDTO, userId, 0.0, websocketKey);
        // 构建标题栏
        Map<String, Integer> dateColMap = new HashMap<>();
        SXSSFWorkbook workbook = new SXSSFWorkbook();
        Boolean buildMonthlyReport = Boolean.TRUE.equals(workHoursSearchVO.getExportMonthlyReport()) && Boolean.TRUE.equals(isOrg);
        if (buildMonthlyReport) {
            buildMonthlyReportTitle(workbook, "工作组月报", WORK_HOURS_CALENDAR_REPORT_LIST, workHoursSearchVO);
            setMonthlyReportData(workbook, projectIds, organizationId, workHoursSearchVO);
        }
        buildExcelTitle(workbook,"工时日历", WORK_HOURS_CALENDAR_LIST, workHoursSearchVO, dateColMap);
        buildExcelTitle(workbook,"未饱和", WORK_HOURS_CALENDAR_LIST, workHoursSearchVO, dateColMap);
        ExcelCursorDTO cursor = new ExcelCursorDTO(2, 0, 100);
        double lastProcess = 0D;
        while (true) {
            // 写入数据
            int sheetNum = 0;
            if(buildMonthlyReport){
                sheetNum = 1;
            }
            PageRequest pageRequest = new PageRequest(cursor.getPage(), cursor.getSize());
            Page<WorkHoursCalendarVO> page = workHoursService.workHoursCalendar(organizationId, projectIds, pageRequest, workHoursSearchVO, isOrg);
            if (!CollectionUtils.isEmpty(page.getContent())) {
                List<WorkHoursCalendarVO> list = page.getContent();
                setData(workbook, list, sheetNum, dateColMap, false, cursor.getRow());
                sheetNum += 1;
                setData(workbook, list, sheetNum, dateColMap, true, cursor.getRow());
                cursor.setRow(cursor.getRow() + page.getContent().size());
                double process = getProcess(cursor.getPage(), page.getTotalPages());
                if (process - lastProcess >= 0.1) {
                    sendProcess(fileOperationHistoryDTO, userId, process, websocketKey);
                    lastProcess = process;
                }
                boolean hasNextPage = (cursor.getPage() + 1) < page.getTotalPages();
                if (!hasNextPage) {
                    break;
                }
            }
        }
        // 上传至minio
        downloadExcel(workbook, excelName, organizationId, websocketKey, userId, fileOperationHistoryDTO);
    }

    private void setMonthlyReportData(SXSSFWorkbook workbook, List<Long> projectIds, Long organizationId, WorkHoursSearchVO workHoursSearchVO) {
       // 查询工作组
        WorkGroupTreeVO workGroupTreeVO = workGroupService.queryWorkGroupTree(organizationId);
        // 查询工时统计数据
        Map<Long, WorkHoursCountVO> countMap = workHoursService.countWorkHoursCalendar(organizationId, projectIds, workHoursSearchVO);
        // 构造excel数据
        List<WorkGroupVO> workGroupVOS = workGroupTreeVO.getWorkGroupVOS().stream()
                                        .filter(v -> !ObjectUtils.isEmpty(v.getId())).collect(Collectors.toList());
        if (CollectionUtils.isEmpty(workGroupVOS)) {
            return;
        }
        // 构造合计数据
        WorkGroupVO total = buildTotalData(workGroupVOS);
        workGroupVOS.add(total);
        // 转换数据对象
        List<WorkGroupReportExcelVO> workGroupReportExcelVOS =  buildWorkGroupReportExcelVOS(workGroupVOS, countMap);
        Map<Long, List<WorkGroupReportExcelVO>> workGroupMap = workGroupReportExcelVOS.stream().collect(Collectors.groupingBy(WorkGroupReportExcelVO::getParentId));
        ExcelCursorDTO excelCursorDTO = new ExcelCursorDTO(2, 0, 0);
        SXSSFSheet sheet = workbook.getSheetAt(0);
        buildData(workbook, workGroupMap.get(0L), sheet, excelCursorDTO, "", workGroupMap);
        // 填写说明信息
        SXSSFRow row = sheet.createRow(excelCursorDTO.getRow());
        SXSSFCell cell = row.createCell(0);
        CellStyle cellStyle = workbook.createCellStyle();
        Font font = workbook.createFont();
        font.setColor(HSSFColor.RED.index);
        font.setFontHeightInPoints((short) 11);
        cellStyle.setFont(font);
        cell.setCellStyle(cellStyle);
        cell.setCellValue("说明：非工作日不计算工时登记饱和情况");
    }

    private WorkGroupVO buildTotalData(List<WorkGroupVO> workGroupVOS) {
        Set<Long> userIds = new HashSet<>();
        workGroupVOS.stream().filter(v -> ObjectUtils.isEmpty(0L)).forEach(v -> {
            if(!CollectionUtils.isEmpty(v.getUserIds())){
                userIds.addAll(v.getUserIds());
            }
        });
        WorkGroupVO workGroupVO = new WorkGroupVO();
        workGroupVO.setUserCount(userIds.size());
        workGroupVO.setName("合计");
        workGroupVO.setUserIds(userIds);
        workGroupVO.setParentId(0L);
        return workGroupVO;
    }

    private List<WorkGroupReportExcelVO> buildWorkGroupReportExcelVOS(List<WorkGroupVO> workGroupVOS, Map<Long, WorkHoursCountVO> countMap) {
        List<WorkGroupReportExcelVO> workGroupReportExcelVOS = new ArrayList<>();
        for (WorkGroupVO workGroupVO : workGroupVOS) {
            WorkGroupReportExcelVO workGroupReportExcelVO = new WorkGroupReportExcelVO(workGroupVO.getId(), workGroupVO.getParentId(),
                    workGroupVO.getName(), workGroupVO.getUserCount(), workGroupVO.getUserIds());
            int actualUserCount = 0;
            int unsaturatedUserCount = 0;
            int unsaturatedTimes = 0;
            Set<Long> userIds = workGroupVO.getUserIds();
            if (!CollectionUtils.isEmpty(userIds)) {
                for (Long userId : userIds) {
                    WorkHoursCountVO workHoursCountVO = countMap.get(userId);
                    if (!ObjectUtils.isEmpty(workHoursCountVO)) {
                        actualUserCount += 1;
                        unsaturatedUserCount += workHoursCountVO.getUnsaturatedUserCount();
                        unsaturatedTimes += workHoursCountVO.getUnsaturatedTimes();
                    }
                }
            }
            workGroupReportExcelVO.setActualUserCount(actualUserCount);
            workGroupReportExcelVO.setUnsaturatedTimes(unsaturatedTimes);
            workGroupReportExcelVO.setUnsaturatedUserCount(unsaturatedUserCount);
            workGroupReportExcelVO.setActualUserProportion(devide(actualUserCount, workGroupVO.getUserCount()));
            workGroupReportExcelVO.setUnsaturatedUserProportion(devide(unsaturatedUserCount, workGroupVO.getUserCount()));
            workGroupReportExcelVOS.add(workGroupReportExcelVO);
        }
        return workGroupReportExcelVOS;
    }

    private String devide(int number1, int number2){
        double result = 0.00;
        if(number2 != 0){
            BigDecimal devide =BigDecimal.valueOf(number1).divide(BigDecimal.valueOf(number2),4, RoundingMode.HALF_UP);
            result = devide.doubleValue();
        }
        NumberFormat percent = NumberFormat.getPercentInstance();
        percent.setMaximumFractionDigits(2);
        return percent.format(result);
    }

    private void buildData(SXSSFWorkbook workbook,
                           List<WorkGroupReportExcelVO> root,
                           SXSSFSheet sheet,
                           ExcelCursorDTO excelCursorDTO,
                           String separate,
                           Map<Long, List<WorkGroupReportExcelVO>> workGroupMap) {
        if (CollectionUtils.isEmpty(root)) {
           return;
        }
        for (WorkGroupReportExcelVO workGroupReportExcelVO : root) {
            workGroupReportExcelVO.setWorkGroupName(separate + workGroupReportExcelVO.getWorkGroupName());
            //样式
            CellStyle cellStyle = workbook.createCellStyle();
            cellStyle.setAlignment(HorizontalAlignment.LEFT);
            if (ObjectUtils.isEmpty(workGroupReportExcelVO.getId())) {
                Font font = workbook.createFont();
                font.setBoldweight(Font.BOLDWEIGHT_BOLD);
                font.setFontHeightInPoints((short) 11);
                cellStyle.setFont(font);
            }
            // 将工作信息填入表格
            ExcelUtil.writeWorkGroupReport(workbook, "工作组月报", WorkGroupReportExcelVO.class, workGroupReportExcelVO, WORK_HOURS_CALENDAR_REPORT_LIST, cellStyle, excelCursorDTO);
            excelCursorDTO.increaseRow();
            List<WorkGroupReportExcelVO> workGroupVOS = workGroupMap.get(workGroupReportExcelVO.getId());
            if (!CollectionUtils.isEmpty(workGroupVOS)) {
                buildData(workbook, workGroupVOS, sheet, excelCursorDTO, "  " + separate, workGroupMap);
            }
        }
    }


    private void buildMonthlyReportTitle(SXSSFWorkbook workbook, String sheetName, List<ExcelTitleVO> workHoursCalendarReportList, WorkHoursSearchVO workHoursSearchVO) {
        // 单元格样式
        CellStyle cellStyle = workbook.createCellStyle();
        cellStyle.setAlignment(HorizontalAlignment.LEFT.getCode());
        cellStyle.setVerticalAlignment(CellStyle.VERTICAL_CENTER);
        cellStyle.setFillPattern(FillPatternType.SOLID_FOREGROUND);
        cellStyle.setFillForegroundColor(HSSFColor.HSSFColorPredefined.PALE_BLUE.getIndex());
        cellStyle.setWrapText(true);
        Font font = workbook.createFont();
        font.setBoldweight(Font.BOLDWEIGHT_BOLD);
        font.setFontHeightInPoints((short) 11);
        cellStyle.setFont(font);

        SXSSFSheet sheet = workbook.createSheet(sheetName);
        //设置默认列宽
        sheet.setDefaultColumnWidth(13);
        SXSSFRow row = sheet.createRow(0);
        // 设置查询时间范围
        SXSSFCell cell = row.createCell(0);
        String dateString = buildDateString(workHoursSearchVO.getStartTime(), workHoursSearchVO.getEndTime());
        cell.setCellValue("时间范围：" + dateString);
        // 生成标题
        SXSSFRow row1 = sheet.createRow(1);
        for (int i = 0; i < workHoursCalendarReportList.size(); i++) {
            SXSSFCell cell1 = row1.createCell(i);
            cell1.setCellStyle(cellStyle);
            ExcelTitleVO excelTitleVO = workHoursCalendarReportList.get(i);
            cell1.setCellValue(excelTitleVO.getTitle());
            sheet.setColumnWidth(i, excelTitleVO.getWidth());
        }
    }

    private String buildDateString(Date startTime, Date endTime) {
        DateFormat df = new SimpleDateFormat(BaseConstants.Pattern.SYS_DATE);
        StringBuilder stringBuilder = new StringBuilder();
        stringBuilder
                .append(df.format(startTime))
                .append("~")
                .append(df.format(endTime));
        return stringBuilder.toString();
    }

    @Override
    @Async
    public void exportWorkHoursCalendarOnOrganizationLevel(Long organizationId, WorkHoursSearchVO workHoursSearchVO, ServletRequestAttributes requestAttributes, Boolean isOrg) {
        List<Long> projectIds = new ArrayList<>();
        Long userId = DetailsHelper.getUserDetails().getUserId();
        workHoursService.handlerProject(organizationId, projectIds, userId, workHoursSearchVO);
        exportWorkHoursCalendar(organizationId, projectIds, workHoursSearchVO, requestAttributes, isOrg);
    }

    @Override
    @Async
    public void exportWorkHoursCalendarOnProjectLevel(Long organizationId, Long projectId, WorkHoursSearchVO workHoursSearchVO, ServletRequestAttributes requestAttributes, Boolean isOrg) {
        exportWorkHoursCalendar(organizationId, Arrays.asList(projectId), workHoursSearchVO, requestAttributes, isOrg);
    }

    @Override
    @Async
    public void exportIssueWorkHoursOnOrganizationLevel(Long organizationId, ServletRequestAttributes currentRequestAttributes, boolean isOrg, SearchVO searchVO, Boolean containsSubIssue) {
        List<Long> projectIds = new ArrayList<>();
        Long userId = DetailsHelper.getUserDetails().getUserId();
        workHoursService.handlePermissionProject(organizationId, projectIds, new ArrayList<>(), userId);
        exportIssueWorkHours(organizationId, projectIds, currentRequestAttributes, isOrg, searchVO, containsSubIssue);
    }

    @Override
    @Async
    public void exportIssueWorkHoursOnProjectLevel(Long organizationId,
                                                   Long projectId,
                                                   ServletRequestAttributes currentRequestAttributes,
                                                   boolean isOrg,
                                                   SearchVO searchVO,
                                                   Boolean containsSubIssue) {
        exportIssueWorkHours(organizationId, Arrays.asList(projectId), currentRequestAttributes, isOrg, searchVO, containsSubIssue);
    }

    @Override
    public void exportIssueWorkHours(Long organizationId,
                                     List<Long> projectIds,
                                     ServletRequestAttributes requestAttributes,
                                     boolean isOrg,
                                     SearchVO searchVO,
                                     boolean containsSubIssue) {
        RequestContextHolder.setRequestAttributes(requestAttributes);
        Long userId = DetailsHelper.getUserDetails().getUserId();
        String websocketKey = buildWebSocketKey(EXPORT_ISSUE_WORK_HOURS, isOrg, organizationId, projectIds.get(0));
        String excelName = buildExcelName(organizationId, "工作项工时", projectIds.get(0), isOrg);
        // 发送websocket
        Long projectId = 0L;
        if (Boolean.FALSE.equals(isOrg)) {
            projectId = projectIds.get(0);
        }
        FileOperationHistoryDTO fileOperationHistoryDTO = initFileOperationHistory(projectId, organizationId, userId, DOING, DOWNLOAD_ISSUE_WORK_FILE, websocketKey);
        sendProcess(fileOperationHistoryDTO, userId, 0.0, websocketKey);
        // 构造标题栏
        List<ObjectSchemeFieldVO> fieldViews = queryFieldViews(organizationId, new HashSet<>(projectIds));
        List<ExcelTitleVO> excelTitleVOS = processExportField(searchVO.getDisplayFields(), fieldViews);
        Workbook workbook = initExcelAndTitle("工作项工时", excelTitleVOS, searchVO);
        // 构建导出数据
        ExcelCursorDTO cursor = new ExcelCursorDTO(2, 0, 100);
        double lastProcess = 0D;
        while(true) {
            List<ExportIssuesVO> exportIssues = new ArrayList<>();
            PageRequest pageRequest = new PageRequest(cursor.getPage(), cursor.getSize());
            Page<ExportIssuesVO> page = handleIssueWorkHoursData(organizationId, projectIds, searchVO, pageRequest, exportIssues, containsSubIssue);
            // 写入excel
            fillExcelCell(exportIssues, excelTitleVOS, cursor,"工作项工时", workbook);
            boolean hasNextPage = (cursor.getPage() + 1) < page.getTotalPages();
            double process = getProcess(cursor.getPage(), page.getTotalPages());
            if (process - lastProcess >= 0.1) {
                sendProcess(fileOperationHistoryDTO, userId, process, websocketKey);
                lastProcess = process;
            }
            if (!hasNextPage) {
                break;
            }
            break;
        }
        //把workbook上传到对象存储服务中
        downloadExcel(workbook, excelName, organizationId, websocketKey, userId, fileOperationHistoryDTO);
    }

    private void fillExcelCell(List<ExportIssuesVO> exportIssues,
                               List<ExcelTitleVO> excelTitleVOS,
                               ExcelCursorDTO cursor,
                               String sheetName,
                               Workbook workbook) {
        CellStyle cellStyle = workbook.createCellStyle();
        cellStyle.setAlignment(HorizontalAlignment.LEFT);
        SXSSFSheet sheet = (SXSSFSheet) workbook.getSheet(sheetName);
        for (ExportIssuesVO exportIssue : exportIssues) {
            ExcelUtil.writeIssueWorkHours(sheet, exportIssue.getClass(), exportIssue, excelTitleVOS, cellStyle, cursor);
            if (Boolean.TRUE.equals(exportIssue.getMergeColumn())) {
                CellRangeAddress cellRangeAddress = new CellRangeAddress(cursor.getRow(), cursor.getRow(), 0, 1);
                sheet.addMergedRegion(cellRangeAddress);
            }
            cursor.increaseRow();
        }
    }

    private Page<ExportIssuesVO> handleIssueWorkHoursData(Long organizationId,
                                          List<Long> projectIds,
                                          SearchVO searchVO,
                                          PageRequest pageRequest,
                                          List<ExportIssuesVO> exportIssuesVOS,
                                          boolean containsSubIssue) {
        String latitude = (String) searchVO.getSearchArgs().get("latitude");
        Page<ExportIssuesVO> page = new Page<>();
        switch (latitude) {
            case "assignee":
                Page<IssueWorkHoursVO> issueWorkHoursVOS = workHoursService.pageQueryAssignee(organizationId, projectIds, pageRequest, searchVO);
                handleAssigneeLatitude(issueWorkHoursVOS.getContent(), exportIssuesVOS, organizationId, projectIds, searchVO, containsSubIssue);
                page = PageUtil.buildPageInfoWithPageInfoList(issueWorkHoursVOS, exportIssuesVOS);
                break;
            case "project":
                Page<IssueWorkHoursVO> pageQueryProject = workHoursService.pageQueryProject(organizationId, projectIds, pageRequest, searchVO);
                handleProjectLatitude(pageQueryProject.getContent(), exportIssuesVOS, organizationId, projectIds, searchVO, containsSubIssue);
                page = PageUtil.buildPageInfoWithPageInfoList(pageQueryProject, exportIssuesVOS);
                break;
            case "projectAssignee":
                Page<IssueWorkHoursVO> project = workHoursService.pageQueryProject(organizationId, projectIds, pageRequest, searchVO);
                handleProjectAssigneeLatitude(project.getContent(), exportIssuesVOS, organizationId, projectIds, searchVO, containsSubIssue);
                page = PageUtil.buildPageInfoWithPageInfoList(project, exportIssuesVOS);
                break;
            default:
                Page<ExportIssuesVO> exportIssuesVOPage = pageQueryIssues(organizationId, projectIds, pageRequest, containsSubIssue, searchVO);
                if (!CollectionUtils.isEmpty(exportIssuesVOPage.getContent())) {
                    sortExportIssueVOS(exportIssuesVOPage.getContent(), exportIssuesVOS);
                }
                page = exportIssuesVOPage;
                break;
        }
        return page;
    }

    private void handleProjectAssigneeLatitude(List<IssueWorkHoursVO> content, List<ExportIssuesVO> exportIssuesVOS, Long organizationId, List<Long> projectIds, SearchVO searchVO, boolean containsSubIssue) {
        if (!CollectionUtils.isEmpty(content)) {
            List<Long> projects = content.stream().map(IssueWorkHoursVO::getProjectId).collect(Collectors.toList());
            // 查询经办人统计信息
            List<IssueWorkHoursVO> assigneeWorkHours = workHoursService.listProjectAssigneeWorkHours(organizationId, projects, searchVO);
            if (CollectionUtils.isEmpty(assigneeWorkHours)) {
                return;
            }
            Map<Long, List<IssueWorkHoursVO>> assigneeWorkHourMap = assigneeWorkHours.stream().collect(Collectors.groupingBy(IssueWorkHoursVO::getProjectId));
            // 查询工作项信息
            SearchVO issueSearchVO = modelMapper.map(searchVO, SearchVO.class);
            Map<String, Object> otherArgs = issueSearchVO.getOtherArgs();
            if (CollectionUtils.isEmpty(otherArgs)) {
                otherArgs = new HashMap<>();
            }
            otherArgs.put("assigneeId", assigneeWorkHours.stream().map(IssueWorkHoursVO::getUserId).collect(Collectors.toSet()));
            issueSearchVO.setOtherArgs(otherArgs);
            Page<ExportIssuesVO> exportIssuesVOPage = pageQueryIssues(organizationId, projects, new PageRequest(0, 0), containsSubIssue, searchVO);
            Map<Long, Map<Long, List<ExportIssuesVO>>> projectAssigneeIssueMap =  buildProjectAssigneeIssue(exportIssuesVOPage.getContent());
            for (IssueWorkHoursVO issueWorkHoursVO : content) {
                ExportIssuesVO exportIssuesVO = buildExportIssue(issueWorkHoursVO, buildProjectName(issueWorkHoursVO.getProjectVO()));
                exportIssuesVOS.add(exportIssuesVO);
                List<IssueWorkHoursVO> assigneeWorkHour = assigneeWorkHourMap.getOrDefault(issueWorkHoursVO.getProjectId(), new ArrayList<>());
                Map<Long, List<ExportIssuesVO>> issueMap = projectAssigneeIssueMap.getOrDefault(issueWorkHoursVO.getProjectId(), new HashMap<>());
                for (IssueWorkHoursVO workHoursVO : assigneeWorkHour) {
                    ExportIssuesVO assignee = buildExportIssue(issueWorkHoursVO, buildUserName(workHoursVO.getUserDTO()));
                    exportIssuesVOS.add(assignee);
                    exportIssuesVOS.addAll(issueMap.getOrDefault(workHoursVO.getUserId(), new ArrayList<>()));
                }
            }
        }
    }

    private Map<Long, Map<Long, List<ExportIssuesVO>>> buildProjectAssigneeIssue(List<ExportIssuesVO> content) {
        Map<Long, Map<Long, List<ExportIssuesVO>>> result = new HashMap<>();
        if(!CollectionUtils.isEmpty(content)){
            Map<Long, List<ExportIssuesVO>> projectExportIssueMap = content.stream().collect(Collectors.groupingBy(ExportIssuesVO::getProjectId));
            for (Map.Entry<Long, List<ExportIssuesVO>> entry : projectExportIssueMap.entrySet()) {
                Long projectId = entry.getKey();
                List<ExportIssuesVO> value = entry.getValue();
                Map<Long, List<ExportIssuesVO>> assigneeMap = buildIssue(value, ExportIssuesVO::getAssigneeId);
                result.put(projectId, assigneeMap);
            }
        }
        return result;
    }

    private void sortExportIssueVOS(List<ExportIssuesVO> content, List<ExportIssuesVO> exportIssuesVOS) {
        Map<Long, List<ExportIssuesVO>> parentIssueMap = content.stream().collect(Collectors.groupingBy(ExportIssuesVO::getParentId));
        List<ExportIssuesVO> roots = parentIssueMap.get(0L);
        for (ExportIssuesVO root : roots) {
            exportIssuesVOS.add(root);
            exportIssuesVOS.addAll(parentIssueMap.getOrDefault(root.getIssueId(), new ArrayList<>()));
        }
    }

    private void handleProjectLatitude(List<IssueWorkHoursVO> content,
                                       List<ExportIssuesVO> exportIssuesVOS,
                                       Long organizationId,
                                       List<Long> projectIds,
                                       SearchVO searchVO,
                                       Boolean containsSubIssue) {
        if (!CollectionUtils.isEmpty(content)) {
            List<Long> projects = content.stream().map(IssueWorkHoursVO::getProjectId).collect(Collectors.toList());
            Page<ExportIssuesVO> exportIssuesVOPage = pageQueryIssues(organizationId, projects, new PageRequest(0, 0), containsSubIssue, searchVO);
            Map<Long, List<ExportIssuesVO>> projectMap = buildIssue(exportIssuesVOPage.getContent(), ExportIssuesVO::getProjectId);
            for (IssueWorkHoursVO issueWorkHoursVO : content) {
                ExportIssuesVO exportIssuesVO = new ExportIssuesVO();
                ProjectVO projectVO = issueWorkHoursVO.getProjectVO();
                exportIssuesVO.setTypeName(buildProjectName(projectVO));
                exportIssuesVO.setWorkTime(issueWorkHoursVO.getWorkTime());
                exportIssuesVO.setMergeColumn(true);
                exportIssuesVO.setCumulativeWorkTime(issueWorkHoursVO.getCumulativeWorkTime());
                exportIssuesVO.setDeviationRate(transformBigDecimal(issueWorkHoursVO.getDeviationRate()));
                exportIssuesVOS.add(exportIssuesVO);
                exportIssuesVOS.addAll(projectMap.getOrDefault(issueWorkHoursVO.getProjectId(), new ArrayList<>()));
            }
        }
    }

    private ExportIssuesVO buildExportIssue(IssueWorkHoursVO issueWorkHoursVO, String typeName){
        ExportIssuesVO exportIssuesVO = new ExportIssuesVO();
        exportIssuesVO.setTypeName(typeName);
        exportIssuesVO.setWorkTime(issueWorkHoursVO.getWorkTime());
        exportIssuesVO.setMergeColumn(true);
        exportIssuesVO.setCumulativeWorkTime(issueWorkHoursVO.getCumulativeWorkTime());
        exportIssuesVO.setDeviationRate(transformBigDecimal(issueWorkHoursVO.getDeviationRate()));
        return exportIssuesVO;
    }

    private String buildProjectName(ProjectVO projectVO) {
        return projectVO.getName() + "(" + projectVO.getCode() + ")";
    }

    private void handleAssigneeLatitude(List<IssueWorkHoursVO> content,
                                        List<ExportIssuesVO> exportIssuesVOS,
                                        Long organizationId,
                                        List<Long> projectIds,
                                        SearchVO searchVO,
                                        Boolean containsSubIssue) {
        if (!CollectionUtils.isEmpty(content)) {
            List<Long> assigneeIds = content.stream().map(IssueWorkHoursVO::getUserId).collect(Collectors.toList());
            // 查询工作项
            SearchVO issueSearchVO = modelMapper.map(searchVO, SearchVO.class);
            Map<String, Object> otherArgs = issueSearchVO.getOtherArgs();
            if (CollectionUtils.isEmpty(otherArgs)) {
                otherArgs = new HashMap<>();
            }
            otherArgs.put("assigneeId", assigneeIds);
            issueSearchVO.setOtherArgs(otherArgs);
            Page<ExportIssuesVO> exportIssuesVOPage = pageQueryIssues(organizationId, projectIds, new PageRequest(0, 0), containsSubIssue, issueSearchVO);
            Map<Long, List<ExportIssuesVO>> assigneeMap = buildIssue(exportIssuesVOPage.getContent(), ExportIssuesVO::getAssigneeId);
            for (IssueWorkHoursVO issueWorkHoursVO : content) {
                 ExportIssuesVO exportIssuesVO = new ExportIssuesVO();
                 UserDTO userDTO = issueWorkHoursVO.getUserDTO();
                 exportIssuesVO.setTypeName(buildUserName(userDTO));
                 exportIssuesVO.setWorkTime(issueWorkHoursVO.getWorkTime());
                 exportIssuesVO.setMergeColumn(true);
                 exportIssuesVO.setCumulativeWorkTime(issueWorkHoursVO.getCumulativeWorkTime());
                 exportIssuesVO.setDeviationRate(transformBigDecimal(issueWorkHoursVO.getDeviationRate()));
                 exportIssuesVOS.add(exportIssuesVO);
                 exportIssuesVOS.addAll(assigneeMap.getOrDefault(issueWorkHoursVO.getUserId(), new ArrayList<>()));
            }
        }
    }

    private Page<ExportIssuesVO> pageQueryIssues(Long organizationId, List<Long> projectIds, PageRequest pageRequest, Boolean containsSubIssue, SearchVO searchVO) {
        Page<IssueDTO> page = workHoursService.pageIssue(organizationId, projectIds, pageRequest, searchVO);
        if(CollectionUtils.isEmpty(page.getContent())){
            return new Page<>();
        }
        List<IssueDTO> issueDTOS = page.getContent();
        Map<String, Object> issueValueMap = new HashMap();
        List<ProjectVO> projectVOS = baseFeignClient.queryByIds(new HashSet<>(projectIds)).getBody();
        Map<Long, String> projectMap = projectVOS.stream().collect(Collectors.toMap(ProjectVO::getId, ProjectVO::getName));
        workHoursService.buildIssueValueMap(organizationId, projectIds, page.getContent(), issueValueMap, searchVO);
        List<ExportIssuesVO> exportIssuesVOS = new ArrayList<>();
        Map<Long, List<IssueTypeVO>> issueTypeDTOMap = (Map<Long, List<IssueTypeVO>>) issueValueMap.getOrDefault("issueTypesMap", new HashMap<>());
        Map<Long, List<Long>> issueMap = new HashMap<>();
        buildIssueMap(issueMap, issueDTOS);
        for (IssueDTO issueDTO : issueDTOS) {
            ExportIssuesVO exportIssuesVO = excelService.buildExcelIssueFromIssue(projectMap.get(issueDTO.getProjectId()), new HashMap<>(), new HashMap<>(), issueValueMap, issueDTO);
            Long parentId = 0L;
            if (Objects.equals("sub_task", issueDTO.getTypeCode())) {
                parentId = ObjectUtils.isEmpty(issueDTO.getParentIssueId()) ? 0L : issueDTO.getParentIssueId();
            }
            if (Objects.equals("bug", issueDTO.getTypeCode()) && !ObjectUtils.isEmpty(issueDTO.getRelateIssueId())) {
                parentId = issueDTO.getRelateIssueId();
            }
            if (ObjectUtils.isEmpty(issueDTO.getAssigneeId())) {
                exportIssuesVO.setAssigneeId(0L);
            }
            exportIssuesVO.setParentId(parentId);
            // 处理问题类型
            List<IssueTypeVO> issueTypeVOS = issueTypeDTOMap.get(issueDTO.getIssueTypeId());
            if (!CollectionUtils.isEmpty(issueTypeVOS)) {
                Map<Long, IssueTypeVO> typeVOMap = issueTypeVOS.stream().collect(Collectors.toMap(IssueTypeVO::getProjectId, Function.identity()));
                IssueTypeVO issueTypeVO = typeVOMap.get(issueDTO.getProjectId());
                if (ObjectUtils.isEmpty(issueTypeVO)) {
                    issueTypeVO = typeVOMap.get(0L);
                }
                exportIssuesVO.setTypeName(issueTypeVO.getName());
            }
            // 处理工时
            handleWorkHours(exportIssuesVO, issueValueMap, new HashMap<>(), containsSubIssue);
            // 处理商业版特有的字段
            if (agilePluginService != null) {
                agilePluginService.setExportIssueBusinessArgs(exportIssuesVO, issueValueMap, issueDTO);
            }
            exportIssuesVOS.add(exportIssuesVO);
        }
        return PageUtil.buildPageInfoWithPageInfoList(page, exportIssuesVOS);
    }

    private void buildIssueMap(Map<Long, List<Long>> issueMap, List<IssueDTO> issueDTOS) {
        issueDTOS.forEach(v -> {
            if (Objects.equals("sub_task", v.getTypeCode())) {
                List<Long> list = issueMap.getOrDefault(v.getParentIssueId(), new ArrayList<>());
                list.add(v.getIssueId());
                issueMap.put(v.getParentIssueId(), list);
            }
            if (Objects.equals("bug", v.getTypeCode()) && !ObjectUtils.isEmpty(v.getRelateIssueId()) && !Objects.equals(0L, v.getRelateIssueId())) {
                List<Long> list = issueMap.getOrDefault(v.getRelateIssueId(), new ArrayList<>());
                list.add(v.getIssueId());
                issueMap.put(v.getRelateIssueId(), list);
            }
        });
    }

    private void handleWorkHours(ExportIssuesVO exportIssuesVO, Map<String, Object> issueValueMap, Map<Long, List<Long>> issueMap, Boolean containsSubIssue) {
        Map<Long, BigDecimal> estimateTimeMap = (Map<Long, BigDecimal>) issueValueMap.getOrDefault("estimateTimeMap", new HashMap<>());
        Map<Long, BigDecimal> allWorkTimeMap = (Map<Long, BigDecimal>) issueValueMap.getOrDefault("allWorkTimeMap", new HashMap<>());
        Map<Long, BigDecimal> workTimeMap = (Map<Long, BigDecimal>) issueValueMap.getOrDefault("workTimeMap", new HashMap<>());
        statisticalWorkHours(exportIssuesVO, issueMap, estimateTimeMap, allWorkTimeMap, workTimeMap, containsSubIssue);
    }

    private void statisticalWorkHours(ExportIssuesVO exportIssuesVO,
                                      Map<Long, List<Long>> issueMap,
                                      Map<Long, BigDecimal> estimateTimeMap,
                                      Map<Long, BigDecimal> allWorkTimeMap,
                                      Map<Long, BigDecimal> workTimeMap,
                                      Boolean containsSubIssue) {
        BigDecimal workTime = workTimeMap.getOrDefault(exportIssuesVO.getIssueId(), BigDecimal.ZERO);
        BigDecimal allWorkTime = allWorkTimeMap.getOrDefault(exportIssuesVO.getIssueId(), BigDecimal.ZERO);
        BigDecimal estimateTime = exportIssuesVO.getEstimateTime();
        if (ObjectUtils.isEmpty(estimateTime)) {
            estimateTime = BigDecimal.ZERO;
        }
        if (containsSubIssue) {
            List<Long> childrens = issueMap.getOrDefault(exportIssuesVO.getIssueId(), new ArrayList<>());
            for (Long children : childrens) {
                BigDecimal childrenWorkTime = workTimeMap.getOrDefault(children, BigDecimal.ZERO);
                BigDecimal childrenAllWorkTime = allWorkTimeMap.getOrDefault(children, BigDecimal.ZERO);
                BigDecimal childrenEstimateTime = estimateTimeMap.getOrDefault(children, BigDecimal.ZERO);
                workTime = workTime.add(childrenWorkTime);
                allWorkTime = allWorkTime.add(childrenAllWorkTime);
                estimateTime = estimateTime.add(childrenEstimateTime);
            }
        }
        // 计算偏差率
        BigDecimal deviationRate = BigDecimal.ZERO;
        if (!Objects.equals(BigDecimal.ZERO, estimateTime)) {
            deviationRate = allWorkTime.subtract(estimateTime).divide(estimateTime, 2,BigDecimal.ROUND_HALF_UP);
        }
        exportIssuesVO.setWorkTime(workTime);
        exportIssuesVO.setEstimateTime(estimateTime);
        exportIssuesVO.setCumulativeWorkTime(allWorkTime);
        exportIssuesVO.setDeviationRate(transformBigDecimal(deviationRate));
    }

    private Map<Long, List<ExportIssuesVO>> buildIssue(List<ExportIssuesVO> list, Function<ExportIssuesVO, Long> function) {
        if (CollectionUtils.isEmpty(list)) {
            return new HashMap<>();
        }
        Map<Long, List<ExportIssuesVO>> result = new HashMap<>();
        Map<Long, ExportIssuesVO> issueMap = list.stream().collect(Collectors.toMap(ExportIssuesVO::getIssueId, Function.identity()));
        Map<Long, List<ExportIssuesVO>> groupMap = list.stream().collect(Collectors.groupingBy(function));
        for (Map.Entry<Long, List<ExportIssuesVO>> entry : groupMap.entrySet()) {
            List<ExportIssuesVO> values = entry.getValue();
            List<Long> existIssue = new ArrayList<>();
            List<ExportIssuesVO> exportIssuesVOS = new ArrayList<>();
            for (ExportIssuesVO value : values) {
                // 子任务和子缺陷需要把需要添加父任务
                Boolean isSubIssue = !ObjectUtils.isEmpty(value) && !Objects.equals(0L, value.getParentId());
                if (isSubIssue && !existIssue.contains(value.getParentId())) {
                    ExportIssuesVO exportIssuesVO = issueMap.get(value.getParentId());
                    if (!ObjectUtils.isEmpty(exportIssuesVO)) {
                        exportIssuesVOS.add(exportIssuesVO);
                        existIssue.add(value.getParentId());
                    }
                }
                if (!existIssue.contains(value.getIssueId())) {
                    exportIssuesVOS.add(value);
                    existIssue.add(value.getIssueId());
                }
            }
            result.put(entry.getKey(), exportIssuesVOS);
        }
        return result;
    }

    private String transformBigDecimal(BigDecimal bigDecimal){
        double result = 0.00;
        if (!ObjectUtils.isEmpty(bigDecimal)) {
            result = bigDecimal.doubleValue();
        }
        NumberFormat percent = NumberFormat.getPercentInstance();
        percent.setMaximumFractionDigits(2);
        return percent.format(bigDecimal);
    }

    private String buildUserName(UserDTO userDTO) {
        String realName = userDTO.getRealName();
        String loginName = Boolean.TRUE.equals(userDTO.getLdap()) ? userDTO.getLoginName() : userDTO.getEmail();
        return realName + "(" + loginName + ")";
    }

    private List<ExcelTitleVO> processExportField(List<ObjectSchemeFieldVO> displayFields, List<ObjectSchemeFieldVO> fieldViews) {
        List<ExcelTitleVO> list = new ArrayList<>();
        if (!CollectionUtils.isEmpty(displayFields)) {
            Map<String, ExcelTitleVO> map = ISSUE_WORK_HOURS_REPORT_LIST.stream().collect(Collectors.toMap(ExcelTitleVO::getCode, Function.identity()));
            Map<Long, ObjectSchemeFieldVO> objectSchemeFieldVOMap = fieldViews.stream().collect(Collectors.toMap(ObjectSchemeFieldVO::getId, Function.identity()));
            for (ObjectSchemeFieldVO displayField : displayFields) {
                ExcelTitleVO excelTitleVO = map.get(displayField.getCode());
                if (!ObjectUtils.isEmpty(excelTitleVO)) {
                    list.add(excelTitleVO);
                } else {
                    ObjectSchemeFieldVO objectSchemeFieldVO = objectSchemeFieldVOMap.get(displayField.getId());
                    if (!ObjectUtils.isEmpty(objectSchemeFieldVO)) {
                        excelTitleVO = new ExcelTitleVO();
                        excelTitleVO.setCode(objectSchemeFieldVO.getCode());
                        excelTitleVO.setTitle(objectSchemeFieldVO.getName());
                        excelTitleVO.setWidth(4000);
                        excelTitleVO.setFieldId(objectSchemeFieldVO.getId());
                        list.add(excelTitleVO);
                    }
                }
            }
        }
        return list;
    }

    private Workbook initExcelAndTitle(String sheetName, List<ExcelTitleVO> list, SearchVO searchVO) {
        SXSSFWorkbook workbook = new SXSSFWorkbook();
        Map<String, Object> searchArgs = searchVO.getSearchArgs();
        Date startTime = null;
        Date endTime = null;
        try {
            startTime = DateUtils.parseDate((String) searchArgs.get("startTime"), BaseConstants.Pattern.DATETIME );
            endTime = DateUtils.parseDate((String) searchArgs.get("endTime"), BaseConstants.Pattern.DATETIME );
        } catch (ParseException e) {
            e.printStackTrace();
        }
        DateFormat df = new SimpleDateFormat(BaseConstants.Pattern.DATE);
        CellStyle style2 = createCellStyle(workbook, (short) 13, HorizontalAlignment.LEFT.getCode(), true);
        CellStyle cellStyle = workbook.createCellStyle();
        cellStyle.setWrapText(true);
        SXSSFSheet sheet = workbook.createSheet(sheetName);
        sheet.setDefaultColumnWidth(13);
        SXSSFRow row1 = sheet.createRow(0);
        row1.setHeight((short) 260);
        // 设置时间范围
        SXSSFCell cell = row1.createCell(0);
        SXSSFCell cell1 = row1.createCell(1);
        cell.setCellValue("时间范围:");
        cell1.setCellValue(df.format(startTime) + "~" + df.format(endTime));
        // 创建标题
        SXSSFRow row2 = sheet.createRow(1);
        for (int i = 0; i < list.size(); ++i) {
            ExcelTitleVO excelTitleVO = list.get(i);
            SXSSFCell cell2 = row2.createCell(i);
            style2.setFillForegroundColor(HSSFColor.HSSFColorPredefined.PALE_BLUE.getIndex());
            style2.setFillPattern(FillPatternType.SOLID_FOREGROUND);
            cell2.setCellStyle(style2);
            cell2.setCellValue(excelTitleVO.getTitle());
            if (!ObjectUtils.isEmpty(excelTitleVO.getWidth())) {
                sheet.setColumnWidth(i, excelTitleVO.getWidth());
            }
        }
        return workbook;
    }

    private CellStyle createCellStyle(Workbook workbook, short fontSize, short aligment, Boolean bold) {
        CellStyle cellStyle = workbook.createCellStyle();
        cellStyle.setAlignment(aligment);
        cellStyle.setVerticalAlignment((short)1);
        Font font = workbook.createFont();
        if (bold) {
            font.setBoldweight((short)700);
        }
        font.setFontHeightInPoints(fontSize);
        cellStyle.setFont(font);
        return cellStyle;
    }

    private List<ObjectSchemeFieldVO> queryFieldViews(Long organizationId, Set<Long> subProjectIds) {
        List<ObjectSchemeFieldDTO> fields = objectSchemeFieldMapper.listByProjectIds(organizationId, new ArrayList<>(subProjectIds), new ObjectSchemeFieldSearchVO(), null);
        List<ObjectSchemeFieldVO> result = new ArrayList<>();
        fields.forEach(f -> {
            ObjectSchemeFieldVO vo = modelMapper.map(f, ObjectSchemeFieldVO.class);
            result.add(vo);
        });
        return result;
    }

    private void setData(Workbook workbook,
                         List<WorkHoursCalendarVO> list,
                         Integer sheetNum,
                         Map<String, Integer> dateColMap,
                         Boolean notSaturated,
                         int startRow) {
        Sheet sheetAt = workbook.getSheetAt(sheetNum);
        for (int i = 0; i < list.size(); i++) {
            WorkHoursCalendarVO workHoursCalendarVO = list.get(i);
            Row row = sheetAt.createRow(startRow);
            Cell cell = row.createCell(0);
            UserMessageDTO userMessageDTO = workHoursCalendarVO.getUserMessageDTO();
            if (!ObjectUtils.isEmpty(userMessageDTO)) {
                cell.setCellValue(userMessageDTO.getName());
            }
            Cell cell1 = row.createCell(1);
            BigDecimal allEstimateTime = workHoursCalendarVO.getAllEstimateTime();
            cell1.setCellValue(ObjectUtils.isEmpty(allEstimateTime) ? null : allEstimateTime.toString());
            Map<String, BigDecimal> countMap = workHoursCalendarVO.getCountMap();
            for (Map.Entry<String, Integer> entry : dateColMap.entrySet()) {
                String key = entry.getKey();
                Integer value = entry.getValue();
                Cell rowCell = row.createCell(value);
                BigDecimal workTime = countMap.getOrDefault(key, BigDecimal.ZERO);
                if (notSaturated && workTime.intValue() >= 8) {
                    continue;
                }
                rowCell.setCellValue(workTime.toString());
            }
            startRow++;
        }
    }

    private void buildExcelTitle(SXSSFWorkbook workbook ,
                                 String sheetName,
                                 List<ExcelTitleVO> list,
                                 WorkHoursSearchVO workHoursSearchVO,
                                 Map<String, Integer> dateColMap) {
        CellStyle cellStyle = workbook.createCellStyle();
        cellStyle.setAlignment(HorizontalAlignment.CENTER.getCode());
        cellStyle.setVerticalAlignment(CellStyle.VERTICAL_CENTER);
        cellStyle.setWrapText(true);
        Font font = workbook.createFont();
        font.setBoldweight(Font.BOLDWEIGHT_BOLD);
        font.setFontHeightInPoints((short) 11);
        cellStyle.setFont(font);

        SXSSFSheet sheet = workbook.createSheet(sheetName);
        //设置默认列宽
        sheet.setDefaultColumnWidth(13);
        SXSSFRow row = sheet.createRow(0);
        SXSSFRow row1 = sheet.createRow(1);
        for (int i = 0; i < list.size(); i++) {
            ExcelTitleVO excelTitleVO = list.get(i);
            SXSSFCell cell = row.createCell(i);
            SXSSFCell cell1 = row1.createCell(i);
            cell.setCellStyle(cellStyle);
            cell.setCellValue(excelTitleVO.getTitle());
            sheet.setColumnWidth(i, excelTitleVO.getWidth());
            CellRangeAddress cellRangeAddress = new CellRangeAddress(0, 1, i, i);
            sheet.addMergedRegion(cellRangeAddress);
        }
        Long days = diffTime(workHoursSearchVO.getStartTime(), DateUtils.addDays(workHoursSearchVO.getEndTime(), 1));
        DateFormat df = new SimpleDateFormat(BaseConstants.Pattern.DATE);
        if (days > 0) {
            int startCol = list.size();
            int endCol = 0;
            Calendar calendar = Calendar.getInstance();
            calendar.setTime(workHoursSearchVO.getStartTime());
            int year = calendar.get(Calendar.YEAR);
            int maxCol = (int) (days + list.size());
            for (int col = list.size(); col < maxCol; col++) {
                SXSSFCell cell = row.createCell(col);
                SXSSFCell cell1 = row1.createCell(col);
                String dateString = buildDate(calendar);
                cell1.setCellValue(dateString);
                int currentYear = calendar.get(Calendar.YEAR);
                endCol = col;
                if (!Objects.equals(year, currentYear) || Objects.equals(col, maxCol - 1)) {
                    if (!Objects.equals(year, currentYear)) {
                        endCol = col - 1;
                    }
                    SXSSFCell cell2 = row.getCell(startCol);
                    cell2.setCellValue(year + "年");
                    if (!Objects.equals(startCol, endCol)) {
                        CellRangeAddress cellRangeAddress = new CellRangeAddress(0, 0, startCol, endCol);
                        sheet.addMergedRegion(cellRangeAddress);
                    }
                    year = currentYear;
                    startCol = col;
                }
                dateColMap.put(df.format(calendar.getTime()), col);
                calendar.add(Calendar.DATE, 1);
            }
        }
    }

    private String buildDate(Calendar calendar) {
        int mouth = calendar.get(Calendar.MONTH) + 1;
        int day = calendar.get(Calendar.DATE);

        StringBuilder stringBuilder = new StringBuilder();
        stringBuilder.append(mouth)
                .append("/")
                .append(day)
                .append("(")
                .append(getWeek(calendar))
                .append(")");
        return  stringBuilder.toString();
    }

    private String  getWeek(Calendar calendar) {
        String[] weeks = {"周日","周一","周二","周三","周四","周五","周六"};
        int weekDays = calendar.get(Calendar.DAY_OF_WEEK);
        return weeks[weekDays-1];
    }

    private Long diffTime(Date startTime, Date endTime) {
        long diff = endTime.getTime() - startTime.getTime();//这样得到的差值是毫秒级别
        return diff / (1000 * 60 * 60 * 24);
    }

    @Override
    @Async
    public void exportWorkHoursLogOnProjectLevel(Long organizationId, List<Long> projectIds, WorkHoursSearchVO workHoursSearchVO, ServletRequestAttributes requestAttributes) {
        exportWorkHoursLog(organizationId, projectIds, workHoursSearchVO, requestAttributes, false);
    }

    @Override
    @Async
    public void exportWorkHoursLogOnOrganizationLevel(Long organizationId, WorkHoursSearchVO workHoursSearchVO, ServletRequestAttributes requestAttributes) {
        List<Long> projectIds = new ArrayList<>();
        Long userId = DetailsHelper.getUserDetails().getUserId();
        workHoursService.handlerProject(organizationId, projectIds, userId, workHoursSearchVO);
        exportWorkHoursLog(organizationId, projectIds, workHoursSearchVO, requestAttributes,true);
    }

    protected Double getProcess(Integer currentNum, Integer totalNum) {
        double process = (currentNum + 1.0) / (totalNum + 1.0) * 0.95 * 100;
        BigDecimal b = BigDecimal.valueOf(process);
        process = b.setScale(1, BigDecimal.ROUND_HALF_UP).doubleValue();
        return process;
    }

    private void buildExportVO(WorkHoursLogVO workHoursLogVO, List<WorkHoursExportVO> workHoursExportVOS) {
        WorkHoursExportVO workHoursExportVO = new WorkHoursExportVO();
        workHoursExportVO.setIssueNum(workHoursLogVO.getIssueNum());
        workHoursExportVO.setSummary(workHoursLogVO.getSummary());
        workHoursExportVO.setWorkTime(workHoursLogVO.getWorkTime());
        workHoursExportVO.setWorkDate(workHoursLogVO.getStartDate());
        UserMessageDTO user = workHoursLogVO.getUser();
        if (!ObjectUtils.isEmpty(user)) {
            workHoursExportVO.setUserName(user.getName());
        }
        IssueTypeVO issueTypeVO = workHoursLogVO.getIssueTypeVO();
        if (!ObjectUtils.isEmpty(issueTypeVO)) {
            workHoursExportVO.setIssueTypeName(issueTypeVO.getName());
        }
        StatusVO statusVO = workHoursLogVO.getStatusVO();
        if (!ObjectUtils.isEmpty(statusVO)) {
            workHoursExportVO.setStatusName(statusVO.getName());
        }
        ProjectVO projectVO = workHoursLogVO.getProjectVO();
        if (!ObjectUtils.isEmpty(projectVO)) {
            workHoursExportVO.setProjectName(projectVO.getName());
        }
        workHoursExportVOS.add(workHoursExportVO);
    }

    public FileOperationHistoryDTO initFileOperationHistory(Long projectId, Long organizationId, Long userId, String status, String action, String websocketKey) {
        FileOperationHistoryDTO fileOperationHistoryDTO = new FileOperationHistoryDTO(projectId, organizationId, userId, action, 0L, 0L, status);
        if (fileOperationHistoryMapper.insertSelective(fileOperationHistoryDTO) != 1) {
            throw new CommonException("error.FileOperationHistoryDTO.insert");
        }
        FileOperationHistoryDTO res = fileOperationHistoryMapper.selectByPrimaryKey(fileOperationHistoryDTO.getId());
        sendProcess(res, userId, 0.0, websocketKey);
        return res;
    }

    public void sendProcess(FileOperationHistoryDTO fileOperationHistoryDTO,
                            Long userId,
                            Double process,
                            String websocketKey) {
        fileOperationHistoryDTO.setProcess(process);
        String message = null;
        try {
            message = objectMapper.writeValueAsString(fileOperationHistoryDTO);
        } catch (JsonProcessingException e) {
            LOGGER.error("object to json error: {0}", e);
        }
        messageClientC7n.sendByUserId(userId, websocketKey, message);
    }

    private void downloadExcel(Workbook workbook,
                               String fileName,
                               Long organizationId,
                               String websocketKey,
                               Long userId,
                               FileOperationHistoryDTO fileOperationHistoryDTO) {
        try (ByteArrayOutputStream os = new ByteArrayOutputStream()) {
            workbook.write(os);
            byte[] content = os.toByteArray();
            MultipartFile file = new MultipartExcel("file", fileName, EXCELCONTENTTYPE, content);

            //返回上载结果
            String path = fileClient.uploadFile(organizationId, "agile", null, fileName, file);
            fileOperationHistoryDTO.setStatus(SUCCESS);
            fileOperationHistoryDTO.setFileUrl(path);
        } catch (Exception e) {
            fileOperationHistoryDTO.setStatus(FAILED);
        } finally {
            try {
                fileOperationHistoryDTO.setLastUpdateDate(new Date());
                fileOperationHistoryMapper.updateByPrimaryKey(fileOperationHistoryDTO);
                sendProcess(fileOperationHistoryDTO, userId, 100.0, websocketKey);
                if (workbook instanceof SXSSFWorkbook) {
                    //处理在磁盘上支持本工作簿的临时文件
                    ((SXSSFWorkbook) workbook).dispose();
                }
                workbook.close();
            } catch (IOException e) {
                LOGGER.warn(EXPORT_ERROR_WORKBOOK_CLOSE, e);
            }
        }
    }

    private String buildExcelName(Long organizationId, String functionName, Long projectId, Boolean isOrg) {
        if (isOrg) {
            OrganizationInfoVO organizationInfoVO = baseFeignClient.query(organizationId).getBody();
            return organizationInfoVO.getTenantName() + "-" + functionName;
        } else {
            ProjectVO projectVO = ConvertUtil.queryProject(projectId);
            return projectVO.getName() + "-" + functionName;
        }
    }

    private String buildWebSocketKey(String exportWorkHoursLog, Boolean isOrg, Long organizationId, Long projectId) {
        if (isOrg) {
            return exportWorkHoursLog + "-org-" + organizationId;
        } else {
            return exportWorkHoursLog + "-" + projectId;
        }
    }
}
