package io.choerodon.agile.app.service.impl;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.app.service.IssueService;
import io.choerodon.agile.app.service.WorkHoursExcelService;
import io.choerodon.agile.app.service.WorkHoursService;
import io.choerodon.agile.infra.dto.ExcelCursorDTO;
import io.choerodon.agile.infra.dto.FileOperationHistoryDTO;
import io.choerodon.agile.infra.dto.UserMessageDTO;
import io.choerodon.agile.infra.feign.BaseFeignClient;
import io.choerodon.agile.infra.feign.vo.OrganizationInfoVO;
import io.choerodon.agile.infra.mapper.FileOperationHistoryMapper;
import io.choerodon.agile.infra.utils.ConvertUtil;
import io.choerodon.agile.infra.utils.DateUtil;
import io.choerodon.agile.infra.utils.ExcelUtil;
import io.choerodon.agile.infra.utils.MultipartExcel;
import io.choerodon.core.client.MessageClientC7n;
import io.choerodon.core.domain.Page;
import io.choerodon.core.exception.CommonException;
import io.choerodon.core.oauth.DetailsHelper;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;
import io.choerodon.mybatis.pagehelper.domain.Sort;
import org.apache.commons.lang3.time.DateUtils;
import org.apache.poi.ss.usermodel.*;
import org.apache.poi.ss.util.CellRangeAddress;
import org.apache.poi.xssf.streaming.SXSSFCell;
import org.apache.poi.xssf.streaming.SXSSFRow;
import org.apache.poi.xssf.streaming.SXSSFSheet;
import org.apache.poi.xssf.streaming.SXSSFWorkbook;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.hzero.boot.file.FileClient;
import org.hzero.core.base.BaseConstants;
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
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.*;
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
    private static final String EXCELCONTENTTYPE = "application/vnd.ms-excel";
    private static final String SUCCESS = "success";
    private static final String FAILED = "failed";
    private static final String DOING = "doing";
    private static final String DOWNLOAD_FILE = "download_file_work_hours_log";
    private static final String DOWNLOAD_CALENDAR_FILE = "download_file_work_hours_calendar";
    private static final List<ExcelTitleVO> WORK_HOURS_LOG_LIST = new ArrayList<>();
    private static final List<ExcelTitleVO> WORK_HOURS_CALENDAR_LIST = new ArrayList<>();
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
    private IssueService issueService;

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
         buildExcelTitle(workbook,"工时日历", WORK_HOURS_CALENDAR_LIST, workHoursSearchVO, dateColMap);
         buildExcelTitle(workbook,"未饱和", WORK_HOURS_CALENDAR_LIST, workHoursSearchVO, dateColMap);
        ExcelCursorDTO cursor = new ExcelCursorDTO(2, 0, 100);
        double lastProcess = 0D;
        while (true) {
            // 写入数据
            PageRequest pageRequest = new PageRequest(cursor.getPage(), cursor.getSize());
            Page<WorkHoursCalendarVO> page = workHoursService.workHoursCalendar(organizationId, projectIds, pageRequest, workHoursSearchVO, isOrg);
            if (!CollectionUtils.isEmpty(page.getContent())) {
                List<WorkHoursCalendarVO> list = page.getContent();
                setData(workbook, list, 0, dateColMap, false, cursor.getRow());
                setData(workbook, list, 1, dateColMap, true, cursor.getRow());
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

    private void handlerProject(Long organizationId, List<Long> projectIds, Long userId, WorkHoursSearchVO workHoursSearchVO){
        if (CollectionUtils.isEmpty(workHoursSearchVO.getProjectIds())) {
            // 查询有权限的项目
            Page<ProjectVO> page = baseFeignClient.pagingProjectsByUserId(organizationId, userId, 0, 0, true, "N_AGILE").getBody();
            if (!CollectionUtils.isEmpty(page.getContent())) {
                projectIds.addAll(page.getContent().stream().map(ProjectVO::getId).collect(Collectors.toList()));
            }
        } else {
            projectIds.addAll(workHoursSearchVO.getProjectIds());
        }
    }


    @Override
    @Async
    public void exportWorkHoursCalendarOnOrganizationLevel(Long organizationId, WorkHoursSearchVO workHoursSearchVO, ServletRequestAttributes requestAttributes, Boolean isOrg) {
        List<Long> projectIds = workHoursSearchVO.getProjectIds();
        Long userId = DetailsHelper.getUserDetails().getUserId();
        handlerProject(organizationId, projectIds, userId, workHoursSearchVO);
        exportWorkHoursCalendar(organizationId, projectIds, workHoursSearchVO, requestAttributes, isOrg);
    }

    @Override
    @Async
    public void exportWorkHoursCalendarOnProjectLevel(Long organizationId, Long projectId, WorkHoursSearchVO workHoursSearchVO, ServletRequestAttributes requestAttributes, Boolean isOrg) {
        exportWorkHoursCalendar(organizationId, Arrays.asList(projectId), workHoursSearchVO, requestAttributes, isOrg);
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
                    CellRangeAddress cellRangeAddress = new CellRangeAddress(0, 0, startCol, endCol);
                    sheet.addMergedRegion(cellRangeAddress);
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
        List<Long> projectIds = workHoursSearchVO.getProjectIds();
        Long userId = DetailsHelper.getUserDetails().getUserId();
        handlerProject(organizationId, projectIds, userId, workHoursSearchVO);
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
