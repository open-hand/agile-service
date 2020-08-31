package io.choerodon.agile.infra.utils;

import io.choerodon.agile.infra.dto.ExcelCursorDTO;
import io.choerodon.agile.infra.dto.PredefinedDTO;
import io.choerodon.agile.infra.enums.ExcelImportTemplateColumn;
import io.choerodon.core.exception.CommonException;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.poi.hssf.usermodel.HSSFWorkbook;
import org.apache.poi.hssf.util.HSSFColor;
import org.apache.poi.ss.usermodel.*;
import org.apache.poi.ss.util.CellRangeAddress;
import org.apache.poi.ss.util.CellRangeAddressList;
import org.apache.poi.xssf.streaming.SXSSFCell;
import org.apache.poi.xssf.streaming.SXSSFRow;
import org.apache.poi.xssf.streaming.SXSSFSheet;
import org.apache.poi.xssf.streaming.SXSSFWorkbook;
import org.apache.poi.xssf.usermodel.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.util.ObjectUtils;
import org.springframework.util.StringUtils;
import org.springframework.web.multipart.MultipartFile;

import javax.servlet.http.HttpServletResponse;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.text.SimpleDateFormat;
import java.util.*;

/**
 * @author dinghuang123@gmail.com
 * @since 2018/8/17
 */
public class ExcelUtil {

    private static final Integer CELL_MAX_LENGTH = 32767;

    public enum Mode {
        SXSSF("SXSSF"), HSSF("HSSF"), XSSF("XSSF");
        private String value;

        Mode(String value) {
            this.value = value;
        }

        public String getValue() {
            return this.value;
        }
    }

    public static class GuideSheet {

        private int rowNum;

        private String fieldName;

        private String requestStr;

        private Boolean hasStyle;

        public GuideSheet(int rowNum, String fieldName, String requestStr, Boolean hasStyle) {
            this.rowNum = rowNum;
            this.fieldName = fieldName;
            this.requestStr = requestStr;
            this.hasStyle = hasStyle;
        }

        public int rowNum() {
            return this.rowNum;
        }

        public String fieldName() {
            return this.fieldName;
        }

        public String requestStr() {
            return this.requestStr;
        }

        public Boolean hasStyle() {
            return this.hasStyle;
        }

    }

    protected ExcelUtil() {
    }

    private static final Logger LOGGER = LoggerFactory.getLogger(ExcelUtil.class);
    private static final String EXCEPTION = "Exception:{}";
    private static final String ERROR_IO_WORKBOOK_WRITE_OUTPUTSTREAM = "error.io.workbook.write.output.stream";

    private static void initGuideSheetByRow(Workbook workbook, Sheet sheet, int rowNum, String fieldName, String requestStr, Boolean hasStyle) {
        CellStyle ztStyle = workbook.createCellStyle();
        Font ztFont = workbook.createFont();
        ztFont.setColor(Font.COLOR_RED);
        ztStyle.setFont(ztFont);
        Row row = sheet.createRow(rowNum);
        row.createCell(0).setCellValue(fieldName);
        Cell cell = row.createCell(1);
        cell.setCellValue(substring(requestStr));
        if (hasStyle) {
            cell.setCellStyle(ztStyle);
        }
    }

    protected static void initGuideSheetRemind(Workbook workbook, Sheet sheet, String remindInfo,
                                             int startRow, int lastRow, int firstCol, int lastCol) {
        CellStyle ztStyle = workbook.createCellStyle();
        Font ztFont = workbook.createFont();
        ztFont.setColor(Font.COLOR_RED);
        ztStyle.setFont(ztFont);
        ztStyle.setAlignment(CellStyle.ALIGN_CENTER);
        ztStyle.setVerticalAlignment(CellStyle.VERTICAL_CENTER);
        sheet.addMergedRegion(new CellRangeAddress(startRow, lastRow, firstCol, lastCol));
        Row row = sheet.getRow(0);
        Cell cell = row.createCell(2);
        cell.setCellValue(substring(remindInfo));
        cell.setCellStyle(ztStyle);
    }

    public static List<GuideSheet> initGuideSheet() {
        int i = 0;
        GuideSheet[] guideSheets = {
                new GuideSheet(i++, "问题类型", "必选项", true),
                new GuideSheet(i++, "所属史诗", "非必选项，普通应用项目未加入项目群ART且问题类型为故事可选，否则不可选", true),
                new GuideSheet(i++, "模块", "非必输项", false),
                new GuideSheet(i++, "冲刺", "非必输项，任务/故事下的子任务冲刺默认和父级一致", false),
                new GuideSheet(i++, "概要", "必输项，限制44个字符以内", true),
                new GuideSheet(i++, "子任务概述", "非必输项，故事、任务类型下可创建子任务", false),
                new GuideSheet(i++, "描述", "非必输，仅支持填写纯文本", false),
                new GuideSheet(i++, "经办人", "非必选项", false),
                new GuideSheet(i++, "报告人", "非必选项", false),
                new GuideSheet(i++, "优先级", "必选项", true),
                new GuideSheet(i++, "预估时间", "非必输项，仅支持3位整数或者0.5，预估时间以小时为单位", false),
                new GuideSheet(i++, "版本", "非必选项", false),
                new GuideSheet(i++, "故事点", "非必输，仅支持3位整数或者0.5，仅故事类型须填写，否则不生效", false),
                new GuideSheet(i++, "史诗名称", "如果问题类型选择史诗，此项必填, 限制10个字符", true),
        };
        return Arrays.asList(guideSheets);
    }

    public static void createGuideSheet(Workbook wb, List<GuideSheet> guideSheets, boolean withFeature) {
        Sheet sheet = initGuide(wb, guideSheets, withFeature);
        initGuideSheetRemind(wb, sheet, "请至下一页，填写信息",0, 9, 2, 4);
        initExample(wb, sheet, withFeature);
    }

    protected static Sheet initGuide(Workbook wb, List<GuideSheet> guideSheets, boolean withFeature) {
        if (withFeature) {
            GuideSheet guideSheet = new GuideSheet(1, "所属特性", "非必须项，普通应用项目加入项目群后且问题类型为故事可选，否则不可选", true);
            guideSheets.set(1, guideSheet);
            guideSheets = guideSheets.subList(0, guideSheets.size() - 1);
        }
        Sheet sheet = wb.createSheet("要求");
        sheet.setColumnWidth(0, 5000);
        sheet.setColumnWidth(1, 17000);

        for (GuideSheet guideSheet : guideSheets) {
            initGuideSheetByRow(wb, sheet, guideSheet.rowNum(),
                    guideSheet.fieldName(), guideSheet.requestStr(), guideSheet.hasStyle());
        }

        sheet.setColumnWidth(2, 3000);
        return sheet;
    }

    private static void initExample(Workbook wb, Sheet sheet, boolean withFeature) {
        sheet.setColumnWidth(ExcelImportTemplateColumn.Issue.SUMMARY_COL, 8000);
        sheet.setColumnWidth(ExcelImportTemplateColumn.Issue.SUB_TASK_COL, 6000);
        sheet.setColumnWidth(ExcelImportTemplateColumn.Issue.DESCRIPTION_COL, 8500);
        sheet.setColumnWidth(ExcelImportTemplateColumn.Issue.PRIORITY_COL, 6000);
        sheet.setColumnWidth(ExcelImportTemplateColumn.Issue.EPIC_NAME_COL, 9000);

        Row row = sheet.createRow(18);
        row.createCell(0).setCellValue("示例：");

        CellStyle blueBackground = wb.createCellStyle();
        blueBackground.setFillForegroundColor(IndexedColors.LIGHT_BLUE.getIndex());
        blueBackground.setFillPattern(FillPatternType.SOLID_FOREGROUND);

        CellStyle coralBackground = wb.createCellStyle();
        coralBackground.setFillForegroundColor(IndexedColors.CORAL.getIndex());
        coralBackground.setFillPattern(FillPatternType.SOLID_FOREGROUND);

        String[] data1 = {"问题类型*", "所属史诗", "模块", "冲刺", "概述*",
                "子任务概述(仅子任务生效)", "描述", "经办人","报告人", "优先级*", "预估时间(小时)",
                "版本", "故事点", "史诗名称(仅问题类型为史诗时生效)"};
        String secondColumnValue = "可以选择史诗";
        if (withFeature) {
            data1[1] = "所属特性";
            secondColumnValue = "可以选择特性";
        }
        int startRow = 20;
        createRow(sheet, startRow++, subArray(data1, withFeature), blueBackground);

        String[] data2 = {"史诗", "", "敏捷管理", "", "请输入史诗的概述",
                "", "请输入导入史诗类型的问题的描述信息", "", "", "高", "", "", "", "导入问题"};
        if (withFeature) {
            data2[0] = "特性";
            data2[4] = "请输入特性的概述";
        }
        createRow(sheet, startRow++, subArray(data2, withFeature), null);

        String[] data3 = {"故事", secondColumnValue, "敏捷管理", "sprint-1", "这里输入故事的概述：故事1",
                "", "导入故事并且导入故事下的子任务", "张三", "张三", "中", "8", "0.1", "2", ""};
        createRow(sheet, startRow++, subArray(data3, withFeature), coralBackground);

        String[] data4 = {"", "", "", "", "", "故事1的子任务1的概述", "请输入子任务1的描述信息", "李四", "李四", "高", "2", "", "", ""};
        createRow(sheet, startRow++, subArray(data4, withFeature), coralBackground);

        String[] data5 = {"", "", "", "", "", "故事1的子任务2的概述", "请输入子任务2的描述信息", "王五", "王五", "中", "4", "", "", ""};
        createRow(sheet, startRow++, subArray(data5, withFeature), coralBackground);

        String[] data6 = {"", "", "", "", "", "故事1的子任务3的概述……", "请输入子任务3的描述信息", "陈七", "陈七", "低", "2", "", "", ""};
        createRow(sheet, startRow++, subArray(data6, withFeature), coralBackground);

        String[] data7 = {"任务", secondColumnValue, "敏捷管理", "sprint-1", "请在此处输入任务的概述：任务1", "", "请输入任务2的描述信息", "王五", "王五", "中", "5", "0.2", "", ""};
        createRow(sheet, startRow++, subArray(data7, withFeature), null);

        String[] data8 = {"", "", "", "", "", "任务1的子任务4的概述", "请输入子任务4的描述信息", "小六", "小六", "中", "2", "0.2", "", ""};
        createRow(sheet, startRow++, subArray(data8, withFeature), null);

        String[] data9 = {"", "", "", "", "", "任务1的子任务5的概述", "请输入子任务5的描述信息", "初八", "初八", "中", "2", "0.2", "", ""};
        createRow(sheet, startRow++, subArray(data9, withFeature), null);


        String[] data10 = {"故事", secondColumnValue, "敏捷管理", "sprint-1", "这里输入故事的概述：故事2", "", "仅导入故事", "张三", "张三", "中", "8", "0.1", "2", ""};
        createRow(sheet, startRow++, subArray(data10, withFeature), coralBackground);

        String[] data11 = {"任务", secondColumnValue, "敏捷管理", "sprint-1", "请在此处输入任务的概述：任务2", "", "请输入任务2的描述信息", "张三", "张三", "中", "8", "0.1", "", ""};
        createRow(sheet, startRow++, subArray(data11, withFeature), null);

        String[] data12 = {"缺陷", secondColumnValue, "敏捷管理", "sprint-1", "请在此处输入缺陷的概述：缺陷1", "", "请输入缺陷2的描述信息", "李四", "李四", "低", "0.5", "0.1", "", ""};
        createRow(sheet, startRow++, subArray(data12, withFeature), coralBackground);
    }

    private static String[] subArray(String[] data, boolean withFeature) {
        if (withFeature) {
            int len = data.length;
            String[] newArray = new String[len - 1];
            for (int i = 0; i < len - 1; i++) {
                newArray[i] = data[i];
            }
            return newArray;
        } else {
            return data;
        }
    }

    public static void createRow(Sheet sheet, int rowNum, String[] data, CellStyle background) {
        Row row = sheet.createRow(rowNum);
        for (int i = 0; i < data.length; i++) {
            Cell cell = row.createCell(i);
            cell.setCellValue(substring(data[i]));
            if (background != null) {
                cell.setCellStyle(background);
            }
        }
    }

    public static Workbook generateExcelAwesome(Workbook generateExcel,
                                                List<Integer> errorRows,
                                                Map<Integer, List<Integer>> errorMapList,
                                                String[] fieldsName,
                                                List<String> priorityList,
                                                List<String> issueTypeList,
                                                List<String> versionList,
                                                String sheetName,
                                                List<String> componentList,
                                                List<String> sprintList,
                                                List<String> users,
                                                PredefinedDTO theSecondColumnPredefined,
                                                boolean withFeature) {
        XSSFWorkbook workbook = new XSSFWorkbook();
        // create guide sheet
        createGuideSheet(workbook, initGuideSheet(), withFeature);
        Sheet resultSheet = workbook.createSheet(sheetName);
        CellStyle style = CatalogExcelUtil.getHeadStyle(workbook);
        Map<Integer,Integer> widthMap = new HashMap<>();
        widthMap.put(ExcelImportTemplateColumn.Issue.EPIC_COL, 8000);
        widthMap.put(ExcelImportTemplateColumn.Issue.SUB_TASK_COL, 8000);
        widthMap.put(ExcelImportTemplateColumn.Issue.EPIC_NAME_COL, 8000);
        generateHeaders(resultSheet, style, Arrays.asList(fieldsName), widthMap);

        List<PredefinedDTO> predefinedList = new ArrayList<>();
        predefinedList.add(
                new PredefinedDTO(
                        priorityList,
                        1,
                        500,
                        ExcelImportTemplateColumn.Issue.PRIORITY_SHEET.getCol(),
                        ExcelImportTemplateColumn.Issue.PRIORITY_SHEET.getCol(),
                        ExcelImportTemplateColumn.Issue.PRIORITY_SHEET.getName(),
                        ExcelImportTemplateColumn.Issue.PRIORITY_SHEET.getIndex()
                ));
        predefinedList.add(
                new PredefinedDTO(
                        issueTypeList,
                        1,
                        500,
                        ExcelImportTemplateColumn.Issue.ISSUE_TYPE_SHEET.getCol(),
                        ExcelImportTemplateColumn.Issue.ISSUE_TYPE_SHEET.getCol(),
                        ExcelImportTemplateColumn.Issue.ISSUE_TYPE_SHEET.getName(),
                        ExcelImportTemplateColumn.Issue.ISSUE_TYPE_SHEET.getIndex()
                ));
        predefinedList.add(
                new PredefinedDTO(
                        versionList,
                        1,
                        500,
                        ExcelImportTemplateColumn.Issue.FIX_VERSION_SHEET.getCol(),
                        ExcelImportTemplateColumn.Issue.FIX_VERSION_SHEET.getCol(),
                        ExcelImportTemplateColumn.Issue.FIX_VERSION_SHEET.getName(),
                        ExcelImportTemplateColumn.Issue.FIX_VERSION_SHEET.getIndex()
                ));
        predefinedList.add(
                new PredefinedDTO(
                        componentList,
                        1,
                        500,
                        ExcelImportTemplateColumn.Issue.COMPONENT_SHEET.getCol(),
                        ExcelImportTemplateColumn.Issue.COMPONENT_SHEET.getCol(),
                        ExcelImportTemplateColumn.Issue.COMPONENT_SHEET.getName(),
                        ExcelImportTemplateColumn.Issue.COMPONENT_SHEET.getIndex()
                ));
        predefinedList.add(
                new PredefinedDTO(
                        sprintList,
                        1,
                        500,
                        ExcelImportTemplateColumn.Issue.SPRINT_SHEET.getCol(),
                        ExcelImportTemplateColumn.Issue.SPRINT_SHEET.getCol(),
                        ExcelImportTemplateColumn.Issue.SPRINT_SHEET.getName(),
                        ExcelImportTemplateColumn.Issue.SPRINT_SHEET.getIndex()
                ));
        predefinedList.add(
                new PredefinedDTO(
                        users,
                        1,
                        500,
                        ExcelImportTemplateColumn.Issue.MANAGER_SHEET.getCol(),
                        ExcelImportTemplateColumn.Issue.MANAGER_SHEET.getCol(),
                        ExcelImportTemplateColumn.Issue.MANAGER_SHEET.getName(),
                        ExcelImportTemplateColumn.Issue.MANAGER_SHEET.getIndex()
                ));
        predefinedList.add(
                new PredefinedDTO(
                        users,
                        1,
                        500,
                        ExcelImportTemplateColumn.Issue.REPORTER_SHEET.getCol(),
                        ExcelImportTemplateColumn.Issue.REPORTER_SHEET.getCol(),
                        ExcelImportTemplateColumn.Issue.REPORTER_SHEET.getName(),
                        ExcelImportTemplateColumn.Issue.REPORTER_SHEET.getIndex()
                ));
        predefinedList.add(theSecondColumnPredefined);

        for (PredefinedDTO predefined : predefinedList) {
            workbook =
                    dropDownList2007(
                            workbook,
                            resultSheet,
                            predefined.values(),
                            predefined.startRow(),
                            predefined.endRow(),
                            predefined.startCol(),
                            predefined.endCol(),
                            predefined.hidden(),
                            predefined.hiddenSheetIndex());
        }

        Sheet sheet = generateExcel.getSheetAt(1);
        int size = sheet.getPhysicalNumberOfRows();
        XSSFCellStyle ztStyle = workbook.createCellStyle();
        Font ztFont = workbook.createFont();
        ztFont.setColor(Font.COLOR_RED);
        ztStyle.setFont(ztFont);
        int index = 1;
        for (int i = 1; i <= size; i++) {
            if (errorRows.contains(i)) {
                Row row = sheet.getRow(i);
                Row newRow = resultSheet.createRow(index++);
                for (int j = 0; j < fieldsName.length; j++) {
                    Cell cell = newRow.createCell(j);
                    if (row.getCell(j) != null) {
                        cell.setCellValue(substring(row.getCell(j).toString()));
                    }
                    if (errorMapList.get(i) != null) {
                        List<Integer> errList = errorMapList.get(i);
                        if (errList.contains(j)) {
                            cell.setCellStyle(ztStyle);
                        }
                    }
                }
            }
        }
        return workbook;
    }

    public static void generateHeaders(Sheet sheet, CellStyle style, List<String> headers,
                                       Map<Integer, Integer> widthMap) {
        Row row = sheet.createRow(0);
        int columnNum = headers.size();
        int defaultWidth = 4000;
        for (int i = 0; i < columnNum; i++) {
            if (!ObjectUtils.isEmpty(widthMap)
                    && !ObjectUtils.isEmpty(widthMap.get(i))) {
                sheet.setColumnWidth(i, widthMap.get(i));
            } else {
                sheet.setColumnWidth(i, defaultWidth);
            }
        }
        for (int i = 0; i < headers.size(); i++) {
            CatalogExcelUtil.initCell(row.createCell(i), style, headers.get(i));
        }
    }


    public static <T> SXSSFWorkbook generateExcel(List<T> list, Class<T> clazz, String[] fieldsName, String[] fields, String sheetName) {
        //1、创建工作簿
        SXSSFWorkbook workbook = new SXSSFWorkbook();
        SimpleDateFormat formatter = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
        if (list != null && !list.isEmpty()) {
            //1.3、列标题样式
            CellStyle style2 = createCellStyle(workbook, (short) 13, CellStyle.ALIGN_LEFT, true);
            //1.4、强制换行
            CellStyle cellStyle = workbook.createCellStyle();
            cellStyle.setWrapText(true);
            //2、创建工作表
            SXSSFSheet sheet = workbook.createSheet(sheetName);
            //设置默认列宽
            sheet.setDefaultColumnWidth(13);
            SXSSFRow row2 = sheet.createRow(0);
            row2.setHeight((short) 260);
            for (int j = 0; j < list.size(); j++) {
                SXSSFRow row = sheet.createRow(j + 1);
                row.setHeight((short) 260);
                Object data = list.get(j);
                for (int i = 0; i < fieldsName.length; i++) {
                    //3.3设置列标题
                    SXSSFCell cell2 = row2.createCell(i);
                    //加载单元格样式
                    cell2.setCellStyle(style2);
                    cell2.setCellValue(fieldsName[i]);
                    //4、操作单元格；将数据写入excel
                    handleWriteCell(row, i, data, cellStyle, fields, clazz, null, formatter);
                }
            }
        }
        return workbook;
    }

    public static Workbook initIssueExportWorkbook(String sheetName, String[] fieldsName) {
        //1、创建工作簿
        SXSSFWorkbook workbook = new SXSSFWorkbook();
        //1.3、列标题样式
        CellStyle style2 = createCellStyle(workbook, (short) 13, CellStyle.ALIGN_LEFT, true);
        //1.4、强制换行
        CellStyle cellStyle = workbook.createCellStyle();
        cellStyle.setWrapText(true);
        //2、创建工作表
        SXSSFSheet sheet = workbook.createSheet(sheetName);
        //设置默认列宽
        sheet.setDefaultColumnWidth(13);
        sheet.setColumnWidth(3, 12000);
        //创建标题列
        SXSSFRow row2 = sheet.createRow(0);
        row2.setHeight((short) 260);
        for (int i = 0; i < fieldsName.length; i++) {
            //3.3设置列标题
            SXSSFCell cell2 = row2.createCell(i);
            //加载单元格样式
            style2.setFillForegroundColor(HSSFColor.PALE_BLUE.index);
            style2.setFillPattern(FillPatternType.SOLID_FOREGROUND);
            cell2.setCellStyle(style2);
            cell2.setCellValue(fieldsName[i]);
        }
        return workbook;
    }

    public static <T> void writeIssue(Map<Long, T> map,
                                      Map<Long,Set<Long>> parentSonMap,
                                      Class<T> clazz,
                                      String[] fieldsName,
                                      String[] fields,
                                      String sheetName,
                                      List<String> autoSizeColumn,
                                      Workbook workbook,
                                      ExcelCursorDTO cursor) {
        //样式
        CellStyle cellStyle = workbook.createCellStyle();
        SXSSFSheet sheet = (SXSSFSheet) workbook.getSheet(sheetName);
        cellStyle.setWrapText(true);

        CellStyle tanForegroundColor = createForegroundColor(workbook, IndexedColors.TAN);
        CellStyle lightTurquoiseForegroundColor = createForegroundColor(workbook, IndexedColors.LIGHT_TURQUOISE);
        SimpleDateFormat formatter = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
        if (!ObjectUtils.isEmpty(map)) {
            Set<Long> childrenSet = new HashSet<>();
            for (Map.Entry<Long, Set<Long>> entry : parentSonMap.entrySet()) {
                childrenSet.addAll(entry.getValue());
            }
            IndexedColors lastColors = null;
            for (Map.Entry<Long, T> entry : map.entrySet()) {
                int rowNum = cursor.getRow();
                Long id = entry.getKey();
                T data = entry.getValue();
                if (childrenSet.contains(id)) {
                    continue;
                }
                Set<Long> sonSet = parentSonMap.get(id);
                boolean hasSonNodes = !ObjectUtils.isEmpty(sonSet);
                CellStyle foregroundColor = null;
                if (hasSonNodes) {
                    if (ObjectUtils.isEmpty(lastColors)) {
                        lastColors = IndexedColors.TAN;
                        foregroundColor = tanForegroundColor;
                    } else if (IndexedColors.TAN.equals(lastColors)) {
                        lastColors = IndexedColors.LIGHT_TURQUOISE;
                        foregroundColor = lightTurquoiseForegroundColor;
                    } else if (IndexedColors.LIGHT_TURQUOISE.equals(lastColors)) {
                        lastColors = IndexedColors.TAN;
                        foregroundColor = tanForegroundColor;
                    }
                }
                fillInExcelRow(clazz, fieldsName, fields, cellStyle, sheet, rowNum, data, foregroundColor, formatter);
                rowNum = cursor.increaseRow();

                if (hasSonNodes) {
                    for (Long sonId : sonSet) {
                        T son = map.get(sonId);
                        if (!ObjectUtils.isEmpty(son)) {
                            fillInExcelRow(clazz, fieldsName, fields, cellStyle, sheet, rowNum, son, foregroundColor, formatter);
                            rowNum = cursor.increaseRow();
                        }
                    }
                }
            }
            sheet.trackAllColumnsForAutoSizing();
            for (int i = 0; i < fieldsName.length; i++) {
                //设置列宽度自适应
                if (autoSizeColumn.contains(fields[i])) {
                    sheet.autoSizeColumn(i);
                }
            }
        }
    }

    public static void writeToResponse(HttpServletResponse response, Workbook workbook) {
        try {
            String disposition = String.format("attachment;filename=\"%s-%s.xlsx\"", "Choerodon", System.currentTimeMillis());
            response.setContentType("application/vnd.ms-excel");
            response.setCharacterEncoding("utf-8");
            response.addHeader("Content-Disposition", disposition);
            workbook.write(response.getOutputStream());
        } catch (Exception e) {
            LOGGER.error(EXCEPTION, e);
        } finally {
            try {
                workbook.close();
            } catch (IOException e) {
                LOGGER.error(EXCEPTION, e);
            }
        }
    }

    protected static CellStyle createForegroundColor(Workbook workbook, IndexedColors colors) {
        CellStyle cellStyle = workbook.createCellStyle();
        cellStyle.setFillForegroundColor(colors.getIndex());
        cellStyle.setFillPattern(FillPatternType.SOLID_FOREGROUND);
        return cellStyle;
    }

    protected static <T> void fillInExcelRow(Class<T> clazz,
                                             String[] fieldsName,
                                             String[] fields,
                                             CellStyle cellStyle,
                                             SXSSFSheet sheet,
                                             int rowNum,
                                             T data,
                                             CellStyle foregroundColor,
                                             SimpleDateFormat formatter) {
        SXSSFRow row = sheet.createRow(rowNum);
        row.setHeight((short) 260);
        for (int i = 0; i < fieldsName.length; i++) {
            //4、操作单元格；将数据写入excel
            handleWriteCell(row, i, data, cellStyle, fields, clazz, foregroundColor, formatter);
        }
    }

    protected static <T> void handleWriteCell(SXSSFRow row,
                                              int i,
                                              Object data,
                                              CellStyle cellStyle,
                                              String[] fields,
                                              Class<T> clazz,
                                              CellStyle foregroundColor,
                                              SimpleDateFormat formatter) {
        SXSSFCell cell = row.createCell(i);
        cell.setCellStyle(cellStyle);
        if (!ObjectUtils.isEmpty(foregroundColor)) {
            cell.setCellStyle(foregroundColor);
        }
        if (data != null) {
            Method method = null;
            try {
                method = clazz.getMethod(createGetter(fields[i]));
            } catch (NoSuchMethodException e) {
                LOGGER.debug("no such method exception: {}", e.getMessage());
                try {
                    method = clazz.getMethod("getFoundationFieldValue");
                } catch (NoSuchMethodException e1) {
                    LOGGER.error(EXCEPTION, e1);
                }
            }
            Object invoke = new Object();
            if (!ObjectUtils.isEmpty(method)) {
                try {
                    invoke = method.invoke(data);
                } catch (InvocationTargetException | IllegalAccessException e) {
                    LOGGER.error(EXCEPTION, e);
                }
            }
            if (invoke instanceof Date) {
                cell.setCellValue(substring(formatter.format(invoke)));
            } else if (invoke instanceof Map) {
                ObjectMapper m = new ObjectMapper();
                Map<String, String> foundationFieldValue = m.convertValue(invoke, Map.class);

                String str = foundationFieldValue.get(fields[i]) != null ? foundationFieldValue.get(fields[i]) : "";
                cell.setCellValue(substring(str));
            } else {
                String str = invoke == null ? null : invoke.toString();
                cell.setCellValue(substring(str));
            }
        } else {
            cell.setCellValue("");
        }

    }

    protected static String substring(String str) {
        if (StringUtils.hasText(str) && str.length() > CELL_MAX_LENGTH) {
            return str.substring(0, CELL_MAX_LENGTH);
        } else {
            return str;
        }
    }


    /**
     * 创建单元格样式
     *
     * @param workbook 工作簿
     * @param fontSize 字体大小
     * @return 单元格样式
     */
    protected static CellStyle createCellStyle(SXSSFWorkbook workbook, short fontSize, short aligment, Boolean bold) {
        CellStyle cellStyle = workbook.createCellStyle();
        cellStyle.setAlignment(aligment);
        //垂直居中
        cellStyle.setVerticalAlignment(CellStyle.VERTICAL_CENTER);
        org.apache.poi.ss.usermodel.Font font = workbook.createFont();
        if (bold) {
            //加粗字体
            font.setBoldweight(org.apache.poi.ss.usermodel.Font.BOLDWEIGHT_BOLD);
        }
        font.setFontHeightInPoints(fontSize);
        cellStyle.setFont(font);
        return cellStyle;
    }

    /**
     * 通过属性名称拼凑getter方法
     *
     * @param fieldName fieldName
     * @return String
     */
    protected static String createGetter(String fieldName) {
        if (fieldName == null || fieldName.length() == 0) {
            return null;
        }
        return "get" + fieldName.substring(0, 1).toUpperCase() + fieldName.substring(1);
    }

    public static Workbook getWorkbookFromMultipartFile(Mode mode, MultipartFile excelFile) {
        try {
            switch (mode) {
                case HSSF:
                    return new HSSFWorkbook(excelFile.getInputStream());
                case XSSF:
                    return new XSSFWorkbook(excelFile.getInputStream());
                default:
                    return null;
            }
        } catch (IOException e) {
            throw new CommonException("error.multipartFile.to.workbook", e);
        }
    }


    public static byte[] getBytes(Workbook workbook) {
        try (ByteArrayOutputStream workbookOutputStream = new ByteArrayOutputStream()) {
            workbook.write(workbookOutputStream);
            return workbookOutputStream.toByteArray();
        } catch (IOException e) {
            throw new CommonException(ERROR_IO_WORKBOOK_WRITE_OUTPUTSTREAM, e);
        }
    }

    /**
     * @param wb               HSSFWorkbook对象
     * @param realSheet        需要操作的sheet对象
     * @param datas            下拉的列表数据
     * @param startRow         开始行
     * @param endRow           结束行
     * @param startCol         开始列
     * @param endCol           结束列
     * @param hiddenSheetName  隐藏的sheet名
     * @param hiddenSheetIndex 隐藏的sheet索引
     * @return
     * @throws Exception
     */
    public static XSSFWorkbook dropDownList2007(Workbook wb, Sheet realSheet, List<String> datas, int startRow, int endRow,
                                                int startCol, int endCol, String hiddenSheetName, int hiddenSheetIndex) {

        XSSFWorkbook workbook = (XSSFWorkbook) wb;
        // 创建一个数据源sheet
        XSSFSheet hidden = workbook.createSheet(hiddenSheetName);
        // 数据源sheet页不显示
        workbook.setSheetHidden(hiddenSheetIndex, true);
        if (datas == null || datas.isEmpty()) {
            return workbook;
        }
        // 将下拉列表的数据放在数据源sheet上
        XSSFRow row = null;
        XSSFCell cell = null;
        for (int i = 0; i < datas.size(); i++) {
            row = hidden.createRow(i);
            cell = row.createCell(0);
            cell.setCellValue(substring(datas.get(i)));
        }
        XSSFDataValidationHelper dvHelper = new XSSFDataValidationHelper((XSSFSheet) realSheet);
        XSSFDataValidationConstraint dvConstraint = (XSSFDataValidationConstraint) dvHelper.createFormulaListConstraint(hiddenSheetName + "!$A$1:$A" + datas.size());
        CellRangeAddressList addressList = null;
        XSSFDataValidation validation = null;
        // 单元格样式
        CellStyle style = workbook.createCellStyle();
        style.setAlignment(CellStyle.ALIGN_CENTER);
        style.setVerticalAlignment(CellStyle.VERTICAL_CENTER);
        // 循环指定单元格下拉数据
        for (int i = startRow; i <= endRow; i++) {
            row = (XSSFRow) realSheet.createRow(i);
            cell = row.createCell(startCol);
            cell.setCellStyle(style);
            addressList = new CellRangeAddressList(i, i, startCol, endCol);
            validation = (XSSFDataValidation) dvHelper.createValidation(dvConstraint, addressList);
            realSheet.addValidationData(validation);
        }

        return workbook;
    }
}
