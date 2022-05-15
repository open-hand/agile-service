package io.choerodon.agile.api.vo.report;

import io.swagger.annotations.ApiModelProperty;

/**
 * @author huaxin.deng@hand-china.com 2021-12-10 11:03:17
 */
public class BacklogPieChartDimensionVO {
    @ApiModelProperty(value = "字段编码")
    private String fieldCode;
    @ApiModelProperty(value = "名称")
    private String name;
    @ApiModelProperty(value = "是否为自定义字段")
    private Boolean customField;
    @ApiModelProperty(value = "字段类型")
    private String fieldType;

    public String getFieldCode() {
        return fieldCode;
    }

    public void setFieldCode(String fieldCode) {
        this.fieldCode = fieldCode;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public Boolean getCustomField() {
        return customField;
    }

    public void setCustomField(Boolean customField) {
        this.customField = customField;
    }

    public String getFieldType() {
        return fieldType;
    }

    public void setFieldType(String fieldType) {
        this.fieldType = fieldType;
    }
}
