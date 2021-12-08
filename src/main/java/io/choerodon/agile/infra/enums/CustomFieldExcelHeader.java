package io.choerodon.agile.infra.enums;

/**
 * @author shinan.chen
 * @date 2018/10/15
 */
public enum CustomFieldExcelHeader {

    //编码
    CODE("编码*", 4000),
    //名称
    NAME("名称*", 5000),
    //字段类型
    FIELD_TYPE("字段类型*", 4000),
    //问题类型
    ISSUE_TYPE("工作项类型*", 5000),
    //字段默认值
    DEFAULT_VALUE("默认值", 6000),
    //字段实际值
    KEY("值", 6000),
    //字段显示值
    VALUE("显示值", 6000),
    //是否启用
    ENABLED("是否启用", 3000),
    ;
    String headerName;
    Integer width;

    CustomFieldExcelHeader(String headerName, Integer width) {
        this.headerName = headerName;
        this.width = width;
    }

    public String getHeaderName() {
        return headerName;
    }

    public Integer getWidth() {
        return width;
    }

    public CustomFieldExcelHeader getHeaderByIndex(int index){
        switch (index){
            case 0:
                return CODE;
            case 1:
                return NAME;
            case 2:
                return FIELD_TYPE;
            case 3:
                return ISSUE_TYPE;
            case 4:
                return DEFAULT_VALUE;
            case 5:
                return KEY;
            case 6:
                return VALUE;
            case 7:
                return ENABLED;
            default: return null;
        }
    }
}
