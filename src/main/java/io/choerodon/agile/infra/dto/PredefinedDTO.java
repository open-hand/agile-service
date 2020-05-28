package io.choerodon.agile.infra.dto;

import java.util.List;

/**
 * excel模版中预定义值
 *
 * @author superlee
 * @since 2020-02-23
 */
public class PredefinedDTO {

    private List<String> values;

    private int startRow;

    private int endRow;

    private int startCol;

    private int endCol;

    private String hidden;

    private int hiddenSheetIndex;

    public PredefinedDTO(List<String> values, int startRow, int endRow,
                         int startCol, int endCol, String hidden, int hiddenSheetIndex) {
        this.values = values;
        this.startRow = startRow;
        this.endRow = endRow;
        this.startCol = startCol;
        this.endCol = endCol;
        this.hidden = hidden;
        this.hiddenSheetIndex = hiddenSheetIndex;
    }

    public List<String> values() {
        return this.values;
    }

    public int startRow() {
        return this.startRow;
    }

    public int endRow() {
        return this.endRow;
    }

    public int startCol() {
        return this.startCol;
    }

    public int endCol() {
        return this.endCol;
    }

    public String hidden() {
        return this.hidden;
    }

    public int hiddenSheetIndex() {
        return this.hiddenSheetIndex;
    }
}
