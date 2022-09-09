package io.choerodon.agile.domain.entity;

import com.alibaba.fastjson.JSONObject;
import io.choerodon.agile.infra.utils.AssertUtilsForCommonException;
import org.springframework.util.ObjectUtils;

import java.util.ArrayList;
import java.util.List;

/**
 * @author superlee
 * @since 2022-09-07
 */
public class ExcelSheetData {

    private Integer rowNum;

    private Integer colNum;

    private JSONObject sheetData;

    public static final String DATE_CELL = "date_cell";
    public static final String STRING_CELL = "string_cell";
    public static final String JSON_KEY_ISSUE_ID = "issueId";
    public static final String JSON_KEY_IS_ERROR = "isError";
    public static final String JSON_KEY_ROW_NUM = "rowNum";

    public ExcelSheetData() {
    }

    public ExcelSheetData(Integer rowNum,
                          Integer colNum,
                          JSONObject sheetData) {
        this.rowNum = rowNum;
        this.colNum = colNum;
        this.sheetData = sheetData;
    }

    public Integer getRowNum() {
        return rowNum;
    }

    public void setRowNum(Integer rowNum) {
        this.rowNum = rowNum;
    }

    public Integer getColNum() {
        return colNum;
    }

    public void setColNum(Integer colNum) {
        this.colNum = colNum;
    }

    public JSONObject getSheetData() {
        return sheetData;
    }

    public void setSheetData(JSONObject sheetData) {
        this.sheetData = sheetData;
    }

    public List<String> queryHeaderList() {
        AssertUtilsForCommonException.notNull(sheetData, "error.excel.sheet.data.sheetData.null");
        AssertUtilsForCommonException.notNull(colNum, "error.excel.sheet.data.colNum.null");
        JSONObject headerRow = (JSONObject) sheetData.get("0");
        List<String> headers = new ArrayList<>();
        for (int i = 0; i <= colNum; i++) {
            JSONObject cellJson = (JSONObject)headerRow.get(i);
            if (ObjectUtils.isEmpty(cellJson)) {
                continue;
            }
            String value = cellJson.getString(STRING_CELL);
            if (!ObjectUtils.isEmpty(value)) {
                headers.add(value);
            }
        }
        return headers;
    }
}
