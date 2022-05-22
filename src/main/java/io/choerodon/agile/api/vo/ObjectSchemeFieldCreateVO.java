package io.choerodon.agile.api.vo;

import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

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
    private String[] context;
    @ApiModelProperty(value = "对象方案编码")
    @NotNull(message = "error.field.schemeCodeNotNull")
    private String schemeCode;
    @ApiModelProperty(value = "字段选项列表")
    private List<FieldOptionUpdateVO> fieldOptions;
    @ApiModelProperty(value = "问题类型id")
    @NotNull(message = "error.field.issueTypeIdNotNull")
    @Encrypt
    private List<Long> issueTypeIds;
    @ApiModelProperty(value = "是否必填")
    private Boolean required;
    @ApiModelProperty(value = "是否在创建页面")
    private Boolean created;
    @ApiModelProperty(value = "是否在编辑页面")
    private Boolean edited;
    @ApiModelProperty(value = "排序值")
    private String rank;

    public String getRank() {
        return rank;
    }

    public void setRank(String rank) {
        this.rank = rank;
    }

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

    public Boolean getRequired() {
        return required;
    }

    public void setRequired(Boolean required) {
        this.required = required;
    }

    public Boolean getCreated() {
        return created;
    }

    public void setCreated(Boolean created) {
        this.created = created;
    }

    public Boolean getEdited() {
        return edited;
    }

    public void setEdited(Boolean edited) {
        this.edited = edited;
    }

    public List<Long> getIssueTypeIds() {
        return issueTypeIds;
    }

    public void setIssueTypeIds(List<Long> issueTypeIds) {
        this.issueTypeIds = issueTypeIds;
    }
}
