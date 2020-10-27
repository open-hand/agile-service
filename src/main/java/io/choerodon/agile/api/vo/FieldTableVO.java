package io.choerodon.agile.api.vo;

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

    private String name;
    private String field;
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
