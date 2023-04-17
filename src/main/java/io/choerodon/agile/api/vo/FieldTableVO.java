package io.choerodon.agile.api.vo;

import io.swagger.annotations.ApiModelProperty;

/**
 * 字段与表名映射vo
 * @author jiaxu.cui@hand-china.com 2020/10/27 上午9:25
 */
public class FieldTableVO {

    public static final String TYPE_BASE = "base";
    public static final String TYPE_PROGRAM = "program";
    public static final String TYPE_BACKLOG = "backlog";
    public static final String TYPE_TRIGGER = "trigger";
    public static final String TYPE_WATERFALL = "waterfall";

    public FieldTableVO() {
    }

    public FieldTableVO(String name, String field, String table) {
        this.name = name;
        this.field = field;
        this.table = table;
    }
    public FieldTableVO(String name,
                        String field,
                        String table,
                        String type) {
        this.name = name;
        this.field = field;
        this.table = table;
        this.type = type;
    }
    @ApiModelProperty(value = "名称")
    private String name;
    @ApiModelProperty(value = "字段")
    private String field;
    @ApiModelProperty(value = "表")
    private String table;

    private String type;

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

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
