package io.choerodon.agile.api.vo;

import java.util.List;

import io.choerodon.mybatis.annotation.ModifyAudit;
import io.choerodon.mybatis.annotation.VersionAudit;

/**
 * @author chihao.ran@hand-china.com
 * 2021/07/13 15:59
 */
@ModifyAudit
@VersionAudit
public class FieldCascadeUpdateVO {
    private Boolean hidden;
    private Boolean required;
    private String defaultValue;
    private Long objectVersionNumber;
    private List<FieldCascadeRuleOptionVO> fieldCascadeRuleOptionList;

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

    public Long getObjectVersionNumber() {
        return objectVersionNumber;
    }

    public void setObjectVersionNumber(Long objectVersionNumber) {
        this.objectVersionNumber = objectVersionNumber;
    }

    public List<FieldCascadeRuleOptionVO> getFieldCascadeRuleOptionList() {
        return fieldCascadeRuleOptionList;
    }

    public void setFieldCascadeRuleOptionList(List<FieldCascadeRuleOptionVO> fieldCascadeRuleOptionList) {
        this.fieldCascadeRuleOptionList = fieldCascadeRuleOptionList;
    }
}
