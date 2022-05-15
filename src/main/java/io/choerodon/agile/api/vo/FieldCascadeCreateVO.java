package io.choerodon.agile.api.vo;

import io.choerodon.agile.infra.enums.IssueConstant;
import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

import java.util.List;
import javax.persistence.Column;
import javax.validation.constraints.NotNull;

/**
 * @author chihao.ran@hand-china.com
 * 2021/07/13 15:59
 */
public class FieldCascadeCreateVO {
    @Encrypt
    @NotNull(message = IssueConstant.ISSUE_CN + "类型不能为空")
    @ApiModelProperty("问题类型id")
    private Long issueTypeId;
    @Encrypt
    @NotNull(message = "字段不能为空")
    @ApiModelProperty("字段id")
    private Long fieldId;
    @Encrypt
    @NotNull(message = "字段选项值不能为空")
    @ApiModelProperty("字段选项id")
    private Long fieldOptionId;
    @Encrypt
    @NotNull(message = "级联字段不能为空")
    @ApiModelProperty("级联字段id")
    private Long cascadeFieldId;
    @Column(name = "is_hidden")
    @ApiModelProperty("是否隐藏")
    private Boolean hidden;
    @Column(name = "is_required")
    @ApiModelProperty("是否必填")
    private Boolean required;
    @ApiModelProperty("默认值")
    private String defaultValue;
    @ApiModelProperty("字段级联规则选项")
    private List<FieldCascadeRuleOptionVO> fieldCascadeRuleOptionList;

    public Long getIssueTypeId() {
        return issueTypeId;
    }

    public void setIssueTypeId(Long issueTypeId) {
        this.issueTypeId = issueTypeId;
    }

    public Long getFieldId() {
        return fieldId;
    }

    public void setFieldId(Long fieldId) {
        this.fieldId = fieldId;
    }

    public Long getFieldOptionId() {
        return fieldOptionId;
    }

    public void setFieldOptionId(Long fieldOptionId) {
        this.fieldOptionId = fieldOptionId;
    }

    public Long getCascadeFieldId() {
        return cascadeFieldId;
    }

    public void setCascadeFieldId(Long cascadeFieldId) {
        this.cascadeFieldId = cascadeFieldId;
    }

    public Boolean getHidden() {
        return hidden;
    }

    public void setHidden(Boolean hidden) {
        this.hidden = hidden;
    }

    public Boolean getRequired() {
        return required;
    }

    public void setRequired(Boolean required) {
        this.required = required;
    }

    public String getDefaultValue() {
        return defaultValue;
    }

    public void setDefaultValue(String defaultValue) {
        this.defaultValue = defaultValue;
    }

    public List<FieldCascadeRuleOptionVO> getFieldCascadeRuleOptionList() {
        return fieldCascadeRuleOptionList;
    }

    public void setFieldCascadeRuleOptionList(List<FieldCascadeRuleOptionVO> fieldCascadeRuleOptionList) {
        this.fieldCascadeRuleOptionList = fieldCascadeRuleOptionList;
    }
}
