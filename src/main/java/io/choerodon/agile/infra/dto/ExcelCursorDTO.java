package io.choerodon.agile.infra.dto;

import java.util.*;

/**
 * @author superlee
 * @since 2020-08-05
 */
public class ExcelCursorDTO {

    private Integer row;

    private Integer page;

    private Integer size;

    private List<Object> collections;

    private boolean previousBorderBold = true;

    private Integer sequence;

    public ExcelCursorDTO() {
        this.collections = new ArrayList<>();
    }

    public ExcelCursorDTO(Integer row, Integer page, Integer size) {
        this.row = row;
        this.page = page;
        this.size = size;
        this.collections = new ArrayList<>();
    }

    public ExcelCursorDTO(Integer row,
                          Integer page,
                          Integer size,
                          Integer sequence) {
        this.row = row;
        this.page = page;
        this.size = size;
        this.sequence = sequence;
        this.collections = new ArrayList<>();
    }

    public Integer getSequence() {
        return sequence;
    }

    public void setSequence(Integer sequence) {
        this.sequence = sequence;
    }

    public boolean isPreviousBorderBold() {
        return previousBorderBold;
    }

    public void setPreviousBorderBold(boolean previousBorderBold) {
        this.previousBorderBold = previousBorderBold;
    }

    public Integer getRow() {
        return row;
    }

    public void setRow(Integer row) {
        this.row = row;
    }

    public Integer increaseRow() {
        return ++row;
    }

    public Integer increasePage() {
        return ++page;
    }

    public Integer getPage() {
        return page;
    }

    public void setPage(Integer page) {
        this.page = page;
    }

    public Integer getSize() {
        return size;
    }

    public void setSize(Integer size) {
        this.size = size;
    }

    public ExcelCursorDTO addCollections(Object object) {
        collections.add(object);
        return this;
    }

    public void clean() {
        collections.forEach(c -> {
            if (c instanceof Collection) {
                ((Collection) c).clear();
            }
            if (c instanceof Map) {
                ((Map) c).clear();
            }
        });
    }
}
