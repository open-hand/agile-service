package io.choerodon.agile.api.vo;

import io.swagger.annotations.ApiModelProperty;

/**
 * @author superlee
 * @since 2020-11-25
 */
public class TableHeaderVO {
    @ApiModelProperty(value = "列")
    private Integer column;
    @ApiModelProperty(value = "名称")
    private String name;
    @ApiModelProperty(value = "字段编码")
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
