package io.choerodon.agile.api.vo;

import java.util.List;

import io.choerodon.mybatis.annotation.ModifyAudit;
import io.choerodon.mybatis.annotation.VersionAudit;
import io.swagger.annotations.ApiModelProperty;

/**
 * @author chihao.ran@hand-china.com
 * 2021/07/13 15:59
 */
@ModifyAudit
@VersionAudit
public class FieldCascadeUpdateVO {
    @ApiModelProperty(value = "是否隐藏")
    private Boolean hidden;
    @ApiModelProperty(value = "是否必填")
    private Boolean required;
    @ApiModelProperty(value = "默认值")
    private String defaultValue;
    @ApiModelProperty(value = "乐观锁")
    private Long objectVersionNumber;
    @ApiModelProperty(value = "字段级联规则选项")
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
