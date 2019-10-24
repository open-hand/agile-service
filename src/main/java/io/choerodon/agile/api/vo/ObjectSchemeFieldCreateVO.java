package io.choerodon.agile.api.vo;

import io.swagger.annotations.ApiModelProperty;

import javax.validation.constraints.NotNull;
import java.util.List;

/**
 * @author shinan.chen
 * @since 2019/3/29
 */
public class ObjectSchemeFieldCreateVO {
    @ApiModelProperty(value = "字段编码")
    @NotNull(message = "error.field.codeNotNull")
    private String code;
    @ApiModelProperty(value = "名称")
    @NotNull(message = "error.field.nameNotNull")
    private String name;
    @ApiModelProperty(value = "默认值")
    private String defaultValue;
    @ApiModelProperty(value = "额外配置（是否当前时间/是否包括小数）")
    private Boolean extraConfig;
    @ApiModelProperty(value = "描述")
    private String description;
    @ApiModelProperty(value = "字段类型")
    @NotNull(message = "error.field.typeNotNull")
    private String fieldType;
    @ApiModelProperty(value = "上下文")
    @NotNull(message = "error.field.contextNotNull")
    private String[] context;
    @ApiModelProperty(value = "对象方案编码")
    @NotNull(message = "error.field.schemeCodeNotNull")
    private String schemeCode;
    @ApiModelProperty(value = "字段选项列表")
    private List<FieldOptionUpdateVO> fieldOptions;

    public Boolean getExtraConfig() {
        return extraConfig;
    }

    public void setExtraConfig(Boolean extraConfig) {
        this.extraConfig = extraConfig;
    }

    public String getDefaultValue() {
        return defaultValue;
    }

    public void setDefaultValue(String defaultValue) {
        this.defaultValue = defaultValue;
    }

    public List<FieldOptionUpdateVO> getFieldOptions() {
        return fieldOptions;
    }

    public void setFieldOptions(List<FieldOptionUpdateVO> fieldOptions) {
        this.fieldOptions = fieldOptions;
    }

    public String getCode() {
        return code;
    }

    public void setCode(String code) {
        this.code = code;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public String getFieldType() {
        return fieldType;
    }

    public void setFieldType(String fieldType) {
        this.fieldType = fieldType;
    }

    public String[] getContext() {
        return context;
    }

    public void setContext(String[] context) {
        this.context = context;
    }

    public String getSchemeCode() {
        return schemeCode;
    }

    public void setSchemeCode(String schemeCode) {
        this.schemeCode = schemeCode;
    }
}
