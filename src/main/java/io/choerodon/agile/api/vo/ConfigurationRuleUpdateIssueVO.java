package io.choerodon.agile.api.vo;

import java.util.List;

/**
 * @author superlee
 * @since 2021-08-05
 */
public class ConfigurationRuleUpdateIssueVO extends ConfigurationRuleSettingVO {

    private List<ConfigurationRuleFieldVO> fields;

    public List<ConfigurationRuleFieldVO> getFields() {
        return fields;
    }

    public void setFields(List<ConfigurationRuleFieldVO> fields) {
        this.fields = fields;
    }
}
