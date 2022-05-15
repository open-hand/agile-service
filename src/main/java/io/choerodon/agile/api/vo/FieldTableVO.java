package io.choerodon.agile.api.vo;

import io.swagger.annotations.ApiModelProperty;

/**
 * 字段与表名映射vo
 * @author jiaxu.cui@hand-china.com 2020/10/27 上午9:25
 */
public class FieldTableVO {

    public FieldTableVO() {
    }

    public FieldTableVO(String name, String field, String table) {
        this.name = name;
        this.field = field;
        this.table = table;
    }
    @ApiModelProperty(value = "名称")
    private String name;
    @ApiModelProperty(value = "字段")
    private String field;
    @ApiModelProperty(value = "表")
    private String table;

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getField() {
        return field;
    }

    public void setField(String field) {
        this.field = field;
    }

    public String getTable() {
        return table;
    }

    public void setTable(String table) {
        this.table = table;
    }
}
