package io.choerodon.agile.api.vo.business;

import io.swagger.annotations.ApiModelProperty;

import java.util.List;
import java.util.Objects;

/**
 * @author superlee
 * @since 2021-08-05
 */
public class ConfigurationRuleUpdateIssueVO extends ConfigurationRuleSettingVO {

    @ApiModelProperty(value = "规则字段集合")
    private List<ConfigurationRuleFieldVO> fields;

    public List<ConfigurationRuleFieldVO> getFields() {
        return fields;
    }

    public void setFields(List<ConfigurationRuleFieldVO> fields) {
        this.fields = fields;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof ConfigurationRuleUpdateIssueVO)) return false;
        ConfigurationRuleUpdateIssueVO that = (ConfigurationRuleUpdateIssueVO) o;
        return Objects.equals(getFields(), that.getFields());
    }

    @Override
    public int hashCode() {
        return Objects.hash(getFields());
    }
}
