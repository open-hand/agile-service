package io.choerodon.agile.api.vo;

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
    @NotNull(message = "问题类型不能为空")
    private Long issueTypeId;
    @Encrypt
    @NotNull(message = "字段不能为空")
    private Long fieldId;
    @Encrypt
    @NotNull(message = "字段选项值不能为空")
    private Long fieldOptionId;
    @Encrypt
    @NotNull(message = "级联字段不能为空")
    private Long cascadeFieldId;
    @Column(name = "is_hidden")
    private Boolean hidden;
    @Column(name = "is_required")
    private Boolean required;
    private String defaultValue;
    private List<FieldCascadeRuleOptionVO> cascadeFieldOptionList;

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

    public List<FieldCascadeRuleOptionVO> getCascadeFieldOptionList() {
        return cascadeFieldOptionList;
    }

    public void setCascadeFieldOptionList(List<FieldCascadeRuleOptionVO> cascadeFieldOptionList) {
        this.cascadeFieldOptionList = cascadeFieldOptionList;
    }
}
