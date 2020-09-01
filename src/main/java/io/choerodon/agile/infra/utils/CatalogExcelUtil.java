package io.choerodon.agile.infra.utils;

import org.apache.poi.hssf.usermodel.*;
import org.apache.poi.hssf.util.HSSFColor;
import org.apache.poi.ss.usermodel.*;

import java.io.IOException;
import java.io.InputStream;

/**
 * Created by HuangFuqiang@choerodon.io on 2019/2/28.
 * Email: fuqianghuang01@gmail.com
 */
public class CatalogExcelUtil {

    protected CatalogExcelUtil () {}

    /**
     * 创建Workbook
     *
     * @param in
     * @return
     * @throws Exception
     */
    public static Workbook createWorkBook(InputStream in) throws IOException {
        try (HSSFWorkbook workbook = new HSSFWorkbook(in)) {
            return workbook;
        }
    }

    /**
     * 获取单单元格字符串值
     *
     * @param cell
     * @return
     */
    public static String getCellStringValue(Cell cell) {
        if (cell == null) {
            return "";
        }

        cell.setCellType(CellType.STRING);
        RichTextString str = cell.getRichStringCellValue();
        return str.getString();
    }

    /**
     * 初始化Excel单元格, 设置单元格值和样式
     *
     * @param cell
     * @param style
     * @param value
     */
    public static void initCell(Cell cell, CellStyle style, String value) {
        cell.setCellStyle(style);
        cell.setCellValue(value);
    }

    /**
     * 初始化Excel单元格, 设置单元格值、样式和备注
     *
     * @param cell
     * @param style
     * @param value
     * @param comment
     */
    public static void initCell(Cell cell, CellStyle style, String value, Comment comment) {
        cell.setCellStyle(style);
        cell.setCellValue(value);
        cell.setCellComment(comment);
    }

    /**
     * 获取Excel单元格备注
     *
     * @param drawing
     * @param anchor
     * @param content
     * @return
     */
    public static Comment getCellComment(Drawing drawing, HSSFClientAnchor anchor, String content) {
        Comment comment = drawing.createCellComment(anchor);
        comment.setString(new HSSFRichTextString(content));
        return comment;
    }

    /**
     * 获取Excel标题单元格样式
     *
     * @param wb
     * @return
     */
    public static CellStyle getHeadStyle(Workbook wb) {
        CellStyle style = wb.createCellStyle();
        style.setFillForegroundColor(HSSFColor.HSSFColorPredefined.PALE_BLUE.getIndex());
        style.setFillPattern(FillPatternType.SOLID_FOREGROUND);
        style.setBorderTop(BorderStyle.THIN);
        style.setBorderRight(BorderStyle.THIN);
        style.setBorderBottom(BorderStyle.THIN);
        style.setBorderLeft(BorderStyle.THIN);

        Font font = wb.createFont();
        // 粗体
        font.setBold(true);
        style.setFont(font);
        style.setLocked(true);
        style.setAlignment(HorizontalAlignment.CENTER);
        return style;
    }

    /**
     * 获取Excel数据单元格样式
     *
     * @param wb
     * @return
     */
    public static CellStyle getBodyStyle(Workbook wb) {
        CellStyle style = wb.createCellStyle();
        style.setBorderTop(BorderStyle.THIN);
        style.setBorderRight(BorderStyle.THIN);
        style.setBorderBottom(BorderStyle.THIN);
        style.setBorderLeft(BorderStyle.THIN);
        return style;
    }

    /**
     * 获取Excel错误单元格样式
     *
     * @param wb
     * @return
     */
    public static CellStyle getErrorStyle(Workbook wb) {
        CellStyle style = wb.createCellStyle();

        Font font = wb.createFont();
        font.setColor(HSSFColor.HSSFColorPredefined.RED.getIndex());

        style.setFont(font);
        return style;
    }
}
