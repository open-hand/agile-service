package io.choerodon.agile.api.vo;

import io.swagger.annotations.ApiModelProperty;

import java.util.List;

/**
 * @author superlee
 * @since 2020-11-11
 */
public class ExcelTemplateVO {
    @ApiModelProperty("系统字段")
    private List<String> systemFields;
    @ApiModelProperty("自定义字段")
    private List<String> customFields;

    public List<String> getSystemFields() {
        return systemFields;
    }

    public void setSystemFields(List<String> systemFields) {
        this.systemFields = systemFields;
    }

    public List<String> getCustomFields() {
        return customFields;
    }

    public void setCustomFields(List<String> customFields) {
        this.customFields = customFields;
    }
}
