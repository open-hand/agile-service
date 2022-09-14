package io.choerodon.agile.infra.utils;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import com.google.common.collect.Maps;
import io.choerodon.core.exception.CommonException;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.CellType;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.util.SheetUtil;

/**
 * Copyright (c) 2022. Hand Enterprise Solution Company. All right reserved.
 *
 * @author zongqi.hao@zknow.com
 * @since 2022/8/30
 */
public class SheetUtils extends SheetUtil {


    /**
     * 获取sheet页第一行列名
     *
     * @param dataSheet sheet页
     * @return 列名集合
     */
    public static Map<Integer, String> getHeaderColumnNames(Sheet dataSheet) {
        Row headerRow = dataSheet.getRow(0);
        if (headerRow == null) {
            throw new CommonException("error.sheet.empty");
        }
        Map<Integer, String> headersMap = Maps.newHashMap();
        for (int i = 0; ; i++) {
            Cell cell = headerRow.getCell(i);
            if (isCellEmpty(cell)) {
                break;
            }
            headersMap.put(i, cell.toString());
        }
        return headersMap;
    }

    /**
     * 删除列实现
     *
     * @param sheet          需要删除的sheet页
     * @param columnToDelete 需要删除的列索引
     */
    public static void deleteColumn(Sheet sheet, int columnToDelete) {
        for (int rId = 0; rId <= sheet.getLastRowNum(); rId++) {
            Row row = sheet.getRow(rId);
            for (int cID = columnToDelete; cID <= row.getLastCellNum(); cID++) {
                Cell cOld = row.getCell(cID);
                if (cOld != null) {
                    row.removeCell(cOld);
                }
                Cell cNext = row.getCell(cID + 1);
                if (cNext != null) {
                    Cell cNew = row.createCell(cID, cNext.getCellTypeEnum());
                    cloneCell(cNew, cNext);
                    if (rId == 0) {
                        sheet.setColumnWidth(cID, sheet.getColumnWidth(cID + 1));
                    }
                }
            }
        }
    }

    /**
     * 右边列左移
     *
     * @param cNew 新cell
     * @param cOld 旧cell
     */
    private static void cloneCell(Cell cNew, Cell cOld) {
        cNew.setCellComment(cOld.getCellComment());
        cNew.setCellStyle(cOld.getCellStyle());

        if (CellType.BOOLEAN == cNew.getCellTypeEnum()) {
            cNew.setCellValue(cOld.getBooleanCellValue());
        } else if (CellType.NUMERIC == cNew.getCellTypeEnum()) {
            cNew.setCellValue(cOld.getNumericCellValue());
        } else if (CellType.STRING == cNew.getCellTypeEnum()) {
            cNew.setCellValue(cOld.getStringCellValue());
        } else if (CellType.ERROR == cNew.getCellTypeEnum()) {
            cNew.setCellValue(cOld.getErrorCellValue());
        } else if (CellType.FORMULA == cNew.getCellTypeEnum()) {
            cNew.setCellValue(cOld.getCellFormula());
        }
    }

    /**
     * 获取真实行数量，通过判断所有列是否为空
     *
     * @param sheet     excel sheet页
     * @param columnNum 列数量
     * @return 真实行数
     */
    public static Integer getRealRowCount(Sheet sheet, int columnNum) {
        Integer count = 0;
        for (int r = 1; r <= sheet.getPhysicalNumberOfRows(); r++) {
            Row row = sheet.getRow(r);
            //row为空跳过
            if (isSkip(row, columnNum)) {
                continue;
            }
            count++;
        }
        return count;
    }

    /**
     * 获取真实行索引，通过判断所有列是否为空
     *
     * @param sheet     excel sheet页
     * @param columnNum 列数量
     * @return 真实行索引
     */
    public static List<Integer> getRealRowList(Sheet sheet, int columnNum) {
        List<Integer> dataRowList = new ArrayList<>();
        for (int r = 1; r <= sheet.getPhysicalNumberOfRows(); r++) {
            Row row = sheet.getRow(r);
            //row为空跳过
            if (isSkip(row, columnNum)) {
                continue;
            }
            dataRowList.add(r);
        }
        return dataRowList;
    }

    public static boolean isSkip(Row row, int columnNum) {
        if (row == null) {
            return true;
        }
        //所有列都为空才跳过
        boolean skip = true;
        for (int i = 0; i < columnNum; i++) {
            Cell cell = row.getCell(i);
            skip = skip && isCellEmpty(cell);

        }
        return skip;
    }

    public static boolean isCellEmpty(Cell cell) {
        return cell == null || cell.toString().equals("") || cell.getCellTypeEnum() == CellType.BLANK;
    }

    public static boolean isCellNotEmpty(Cell cell) {
        return !isCellEmpty(cell);
    }
}
