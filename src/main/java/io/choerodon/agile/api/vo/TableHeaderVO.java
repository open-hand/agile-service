package io.choerodon.agile.api.vo;

/**
 * @author superlee
 * @since 2020-11-25
 */
public class TableHeaderVO {

    private Integer column;

    private String name;

    private String fieldCode;

    public TableHeaderVO() {
    }

    public TableHeaderVO(Integer column, String name, String fieldCode) {
        this.column = column;
        this.name = name;
        this.fieldCode = fieldCode;
    }


    public Integer getColumn() {
        return column;
    }

    public void setColumn(Integer column) {
        this.column = column;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getFieldCode() {
        return fieldCode;
    }

    public void setFieldCode(String fieldCode) {
        this.fieldCode = fieldCode;
    }
}
