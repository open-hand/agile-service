package io.choerodon.agile.infra.utils;
import io.choerodon.agile.api.vo.business.ExportIssuesVO;
import io.choerodon.agile.infra.dto.ExcelCursorDTO;
import io.choerodon.agile.infra.enums.ExcelImportTemplate;
import io.choerodon.agile.infra.enums.FieldCode;
import io.choerodon.agile.infra.enums.IssueConstant;
import io.choerodon.core.exception.CommonException;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.poi.hssf.usermodel.HSSFWorkbook;
import org.apache.poi.hssf.util.CellReference;
import org.apache.poi.hssf.util.HSSFColor;
import org.apache.poi.ss.usermodel.*;
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
import java.io.InputStream;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.math.BigDecimal;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.stream.Collectors;

/**
 * @author dinghuang123@gmail.com
 * @since 2018/8/17
 */
public class ExcelUtil {

    private static final Integer CELL_MAX_LENGTH = 32767;
    protected static final List<String> dateFormatterFiled = Arrays.asList(FieldCode.ACTUAL_START_TIME, FieldCode.ACTUAL_END_TIME, FieldCode.ESTIMATED_START_TIME, FieldCode.ESTIMATED_END_TIME);
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

    private static final Map<String, Integer> WIDTH_MAP = new HashMap<>();
    static {
        Arrays.asList("概要", "Tag", "关联" + IssueConstant.ISSUE_CN).forEach(x -> WIDTH_MAP.put(x, 8000));
        Arrays.asList("描述").forEach(x -> WIDTH_MAP.put(x, 12000));
        Arrays.asList("冲刺").forEach(x -> WIDTH_MAP.put(x, 6000));
        Arrays.asList("版本", "修复的版本", "冲刺", "影响的版本", "所属史诗", "创建时间", "最后更新时间", "预计开始时间", "预计结束时间", "已耗费时间", "当前预估时间")
                .forEach(x -> WIDTH_MAP.put(x, 5000));
    }


    private static String[] subArray(String[] data, boolean withFeature) {
        if (withFeature) {
            return Arrays.copyOf(data,data.length);
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


    public static void generateHeaders(Sheet sheet, CellStyle style, List<String> headers) {
        Row row = sheet.createRow(0);
        int defaultWidth = 4000;
        for (int i = 0; i < headers.size(); i++) {
            String value = headers.get(i);
            Integer width = ExcelImportTemplate.IssueHeader.getWidthByValue(value);
            if (width == null) {
                sheet.setColumnWidth(i, defaultWidth);
            } else {
                sheet.setColumnWidth(i, width);
            }
            CatalogExcelUtil.initCell(row.createCell(i), style, headers.get(i));
        }
    }




    public static Workbook initIssueExportWorkbook(String sheetName, String[] fieldsName) {
        //1、创建工作簿
        SXSSFWorkbook workbook = new SXSSFWorkbook();
        //1.3、列标题样式
        CellStyle style2 = createCellStyle(workbook, (short) 13, HorizontalAlignment.LEFT.getCode(), true);
        //1.4、强制换行
        CellStyle cellStyle = workbook.createCellStyle();
        cellStyle.setWrapText(true);
        //2、创建工作表
        SXSSFSheet sheet = workbook.createSheet(sheetName);
        //设置默认列宽
        sheet.setDefaultColumnWidth(13);
        //创建标题列
        SXSSFRow row2 = sheet.createRow(0);
        row2.setHeight((short) 260);
        for (int i = 0; i < fieldsName.length; i++) {
            //3.3设置列标题
            SXSSFCell cell2 = row2.createCell(i);
            //加载单元格样式
            style2.setFillForegroundColor(HSSFColor.HSSFColorPredefined.PALE_BLUE.getIndex());
            style2.setFillPattern(FillPatternType.SOLID_FOREGROUND);
            cell2.setCellStyle(style2);
            cell2.setCellValue(fieldsName[i]);
            Integer width = WIDTH_MAP.get(fieldsName[i]);
            if (width != null) {
                sheet.setColumnWidth(i, width);
            }
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
                    if (ObjectUtils.isEmpty(lastColors) || IndexedColors.LIGHT_TURQUOISE.equals(lastColors)) {
                        lastColors = IndexedColors.TAN;
                        foregroundColor = tanForegroundColor;
                    } else if (IndexedColors.TAN.equals(lastColors)) {
                        lastColors = IndexedColors.LIGHT_TURQUOISE;
                        foregroundColor = lightTurquoiseForegroundColor;
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
        cellStyle.setAlignment(HorizontalAlignment.LEFT);
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
        short height = (short) 300;
        if (data instanceof ExportIssuesVO) {
            ExportIssuesVO vo = (ExportIssuesVO) data;
            int size = vo.getRelatedIssueCount();
            int result = height * size;
            if (result > Short.MAX_VALUE) {
                result = Short.MAX_VALUE;
            }
            height = (short) result;
        }
        row.setHeight(height);
        SimpleDateFormat dateFormatter = new SimpleDateFormat("yyyy-MM-dd HH:mm");
        for (int i = 0; i < fieldsName.length; i++) {
            //4、操作单元格；将数据写入excel
            Boolean formatDate = dateFormatterFiled.contains(fields[i]);
            handleWriteCell(row, i, data, cellStyle, fields, clazz, foregroundColor, formatDate ? dateFormatter : formatter);
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
            } else if (invoke instanceof BigDecimal) {
                cell.setCellValue(ObjectUtils.isEmpty(invoke) ? null : ((BigDecimal) invoke).doubleValue());
            } else if (invoke instanceof Integer) {
                cell.setCellValue(ObjectUtils.isEmpty(invoke) ? null : ((Integer) invoke).doubleValue());
            } else {
                String str = invoke == null ? null : invoke.toString();
                cell.setCellValue(substring(str));
            }
        } else {
            cell.setCellValue("");
        }

    }

    public static String substring(String str) {
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
    protected static CellStyle createCellStyle(Workbook workbook, short fontSize, short aligment, Boolean bold) {
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

    protected static <T> void handleWriteCell(SXSSFRow row,
                                              int i,
                                              Object data,
                                              CellStyle cellStyle,
                                              String field,
                                              Class<T> clazz,
                                              SimpleDateFormat simpleDateFormat) {
        SXSSFCell cell = row.createCell(i);
        if (data != null) {
            Method method = null;
            try {
                method = clazz.getMethod(createGetter(field));
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
                cell.setCellValue(substring(simpleDateFormat.format(invoke)));
            } else if (invoke instanceof Map) {
                ObjectMapper m = new ObjectMapper();
                Map<String, String> foundationFieldValue = m.convertValue(invoke, Map.class);

                String str = foundationFieldValue.get(field) != null ? foundationFieldValue.get(field) : "";
                cell.setCellValue(substring(str));
            } else if (invoke instanceof BigDecimal) {
                cell.setCellValue(ObjectUtils.isEmpty(invoke) ? null : ((BigDecimal) invoke).doubleValue());
            } else if (invoke instanceof Integer) {
                cell.setCellValue(ObjectUtils.isEmpty(invoke) ? null : ((Integer) invoke).doubleValue());
            } else {
                String str = invoke == null ? null : invoke.toString();
                cell.setCellValue(substring(str));
            }
        } else {
            cell.setCellValue("");
        }
        cell.setCellStyle(cellStyle);

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

    public static Workbook getWorkbookFromInputStream(Mode mode, InputStream inputStream) {
        try {
            switch (mode) {
                case HSSF:
                    return new HSSFWorkbook(inputStream);
                case XSSF:
                    return new XSSFWorkbook(inputStream);
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
        int dateSize = datas.size();
        String columnLetter = CellReference.convertNumToColString(startCol);
        CellRangeAddressList addressList = null;
        XSSFDataValidation validation = null;
        // 单元格样式
        CellStyle style = workbook.createCellStyle();
        style.setAlignment(CellStyle.ALIGN_CENTER);
        style.setVerticalAlignment(CellStyle.VERTICAL_CENTER);
        // 循环指定单元格下拉数据
        for (int i = startRow; i <= endRow; i++) {
            XSSFDataValidationConstraint dvConstraint = buildDataValidationConstraint(columnLetter, hiddenSheetName, i, dateSize, dvHelper);
            row = (XSSFRow) realSheet.createRow(i);
            cell = row.createCell(startCol);
            cell.setCellStyle(style);
            addressList = new CellRangeAddressList(i, i, startCol, endCol);
            validation = (XSSFDataValidation) dvHelper.createValidation(dvConstraint, addressList);
            realSheet.addValidationData(validation);
        }

        return workbook;
    }

    /**
     * 公式讲解参考
     * @see <a href="https://zhuanlan.zhihu.com/p/38156200">参考</a>
     * @param columnLetter
     * @param hiddenSheetName
     * @param rowNum
     * @param dateSize
     * @param dvHelper
     * @return
     */
    private static XSSFDataValidationConstraint buildDataValidationConstraint(String columnLetter,
                                                                              String hiddenSheetName,
                                                                              int rowNum,
                                                                              int dateSize,
                                                                              XSSFDataValidationHelper dvHelper) {
        int realRow = rowNum + 1;
        StringBuilder formulaBuilder = new StringBuilder();
        formulaBuilder
                .append("OFFSET(")
                .append(hiddenSheetName)
                .append("!$A1, ")
                .append("MATCH($")
                .append(columnLetter)
                .append(realRow)
                .append("&\"*\", ")
                .append(hiddenSheetName)
                .append("!$A1:$A")
                .append(dateSize)
                .append(", 0) - 1, 0, COUNTIF(")
                .append(hiddenSheetName)
                .append("!$A1:$A")
                .append(dateSize)
                .append(", $")
                .append(columnLetter)
                .append(realRow)
                .append("&\"*\"), 1)");
        String listFormula = formulaBuilder.toString();
        return (XSSFDataValidationConstraint) dvHelper.createFormulaListConstraint(listFormula);
    }

    /**
     * 不同workbook 的sheet 复制，复制数据、样式<br/>
     *
     * <br/>建议用于 不同book间复制，同时复制数据和样式<br/>
     * eg: copySheet(srcSheet, desSheet, mapping)
     *
     *
     * @see <a href="https://segmentfault.com/a/1190000015284947">只参考了不同workbook的sheet复制，如果需要其他方法请参考原文</a>
     * @param mapping 不同文件间复制时，如果要复制样式，必传，否则不复制样式
     */
    public static void copySheet(Sheet srcSheet, Sheet desSheet, StyleMapping mapping) {
        copySheet(srcSheet, desSheet, true, true, mapping);
    }

    /**
     * 把一个excel中的styleTable复制到另一个excel中<br>
     * 如果是同一个excel文件，就不用复制styleTable了
     * @return StyleMapping 两个文件中styleTable的映射关系
     * @see StyleMapping
     */
    public static StyleMapping copyCellStyle(Workbook srcBook, Workbook desBook){
        if (null == srcBook || null == desBook) {
            throw new CommonException("源excel 或 目标excel 不存在");
        }
        if (srcBook.equals(desBook)) {
            throw new CommonException("不要使用此方法在同一个文件中copy style，同一个excel中复制sheet不需要copy Style");
        }
        if ((srcBook instanceof HSSFWorkbook && desBook instanceof XSSFWorkbook) ||
                (srcBook instanceof XSSFWorkbook && desBook instanceof HSSFWorkbook)) {
            throw new CommonException("不支持在不同的版本的excel中复制样式）");
        }

        short[] src2des = new short[srcBook.getNumCellStyles()];
        short[] des2src = new short[desBook.getNumCellStyles() + srcBook.getNumCellStyles()];

        for(short i=0;i<srcBook.getNumCellStyles();i++){
            //建立双向映射
            CellStyle srcStyle = srcBook.getCellStyleAt(i);
            CellStyle desStyle = desBook.createCellStyle();
            src2des[srcStyle.getIndex()] = desStyle.getIndex();
            des2src[desStyle.getIndex()] = srcStyle.getIndex();

            //复制样式
            desStyle.cloneStyleFrom(srcStyle);
        }


        return new StyleMapping(des2src, src2des);
    }


    /**
     * sheet 复制, 灵活控制是否控制数据、样式<br/>
     *
     * <br/>不建议直接使用
     *
     * @param copyValueFlag 控制是否复制数据
     * @param copyStyleFlag 控制是否复制样式
     * @param mapping       不同book中复制样式时，必传
     */
    private static void copySheet(Sheet srcSheet, Sheet desSheet, boolean copyValueFlag, boolean copyStyleFlag, StyleMapping mapping) {
        //合并区域处理
        copyMergedRegion(srcSheet, desSheet);

        //行复制
        Iterator<Row> rowIterator = srcSheet.rowIterator();

        int areadlyColunm = 0;
        while (rowIterator.hasNext()) {
            Row srcRow = rowIterator.next();
            Row desRow = desSheet.createRow(srcRow.getRowNum());
            copyRow(srcRow, desRow, copyValueFlag, copyStyleFlag, mapping);

            //调整列宽(增量调整)
            if (srcRow.getPhysicalNumberOfCells() > areadlyColunm) {
                for (int i = areadlyColunm; i < srcRow.getPhysicalNumberOfCells(); i++) {
                    desSheet.setColumnWidth(i, srcSheet.getColumnWidth(i));
                }
                areadlyColunm = srcRow.getPhysicalNumberOfCells();
            }
        }
    }

    /**
     * 复制行
     */
    private static void copyRow(Row srcRow, Row desRow,boolean copyValueFlag, boolean copyStyleFlag, StyleMapping mapping) {
        Iterator<Cell> it = srcRow.cellIterator();
        while (it.hasNext()) {
            Cell srcCell = it.next();
            Cell desCell = desRow.createCell(srcCell.getColumnIndex());
            copyCell(srcCell, desCell, copyValueFlag, copyStyleFlag, mapping);
        }
    }

    /**
     * 复制单元格
     * @param copyValueFlag 控制是否复制单元格的内容
     * @param copyStyleFlag 控制是否复制样式
     * @param mapping 不同文件间复制时，如果需要连带样式复制，必传，否则不复制样式
     */
    private static void copyCell(Cell srcCell, Cell desCell, boolean copyValueFlag, boolean copyStyleFlag, StyleMapping mapping) {
        Workbook srcBook = srcCell.getSheet().getWorkbook();
        Workbook desBook = desCell.getSheet().getWorkbook();

        //复制样式
        //如果是同一个excel文件内，连带样式一起复制
        if (srcBook == desBook && copyStyleFlag) {
            //同文件，复制引用
            desCell.setCellStyle(srcCell.getCellStyle());
        } else if (copyStyleFlag && !ObjectUtils.isEmpty(mapping)) {
            //不同文件，通过映射关系复制
            short desIndex = mapping.desIndex(srcCell.getCellStyle().getIndex());
            desCell.setCellStyle(desBook.getCellStyleAt(desIndex));
        }

        //复制评论
        if (srcCell.getCellComment() != null) {
            desCell.setCellComment(srcCell.getCellComment());
        }

        //复制内容
        desCell.setCellType(srcCell.getCellTypeEnum());

        if (copyValueFlag) {
            switch (srcCell.getCellTypeEnum()) {
                case STRING:
                    desCell.setCellValue(srcCell.getStringCellValue());
                    break;
                case NUMERIC:
                    desCell.setCellValue(srcCell.getNumericCellValue());
                    break;
                case FORMULA:
                    desCell.setCellFormula(srcCell.getCellFormula());
                    break;
                case BOOLEAN:
                    desCell.setCellValue(srcCell.getBooleanCellValue());
                    break;
                case ERROR:
                    desCell.setCellValue(srcCell.getErrorCellValue());
                    break;
                case BLANK:
                    //nothing to do
                    break;
                default:
                    break;
            }
        }

    }

    /**
     * 复制区域（合并单元格）
     */
    private static void copyMergedRegion(Sheet srcSheet, Sheet desSheet) {
        int sheetMergerCount = srcSheet.getNumMergedRegions();
        for (int i = 0; i < sheetMergerCount; i++) {
            desSheet.addMergedRegion(srcSheet.getMergedRegion(i));
        }
    }

    /**
     * 存放两个excel文件中的styleTable的映射关系，以便于在复制表格时，在目标文件中获取到对应的样式
     */
    public static class StyleMapping {
        /**
         *
         */
        private short[] des2srcIndexMapping;
        /**
         *
         */
        private short[] src2desIndexMapping;

        /**
         * 不允许其他类创建此类型对象
         */
        private StyleMapping() {
        }

        public StyleMapping(short[] des2srcIndexMapping, short[] src2desIndexMapping) {
            this.des2srcIndexMapping = des2srcIndexMapping;
            this.src2desIndexMapping = src2desIndexMapping;
        }

        public short srcIndex(short desIndex) {
            if (desIndex < 0 || desIndex >= this.des2srcIndexMapping.length) {
                throw new CommonException("索引越界：源文件styleNum=" + this.des2srcIndexMapping.length + " 访问位置=" + desIndex);
            }
            return this.des2srcIndexMapping[desIndex];
        }

        /**
         * 根据源文件的style的index,获取目标文件的style的index
         * @param srcIndex 源excel中style的index
         * @return desIndex 目标excel中style的index
         */
        public short desIndex(short srcIndex) {
            if (srcIndex < 0 || srcIndex >= this.src2desIndexMapping.length) {
                throw new CommonException("索引越界：源文件styleNum=" + this.src2desIndexMapping.length + " 访问位置=" + srcIndex);
            }

            return this.src2desIndexMapping[srcIndex];
        }
    }

}
