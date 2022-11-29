package io.choerodon.agile.api.vo.search;

import java.util.List;

import org.hzero.starter.keyencrypt.core.Encrypt;

/**
 * @author superlee
 * @since 2022-11-29
 */
public class EmptyCondition {

    private List<String> predefinedFieldCode;
    @Encrypt
    private List<Long> customFieldIds;

    public List<String> getPredefinedFieldCode() {
        return predefinedFieldCode;
    }

    public void setPredefinedFieldCode(List<String> predefinedFieldCode) {
        this.predefinedFieldCode = predefinedFieldCode;
    }

    public List<Long> getCustomFieldIds() {
        return customFieldIds;
    }

    public void setCustomFieldIds(List<Long> customFieldIds) {
        this.customFieldIds = customFieldIds;
    }

    @Override
    public String toString() {
        return "EmptyCondition{" +
                "predefinedFieldCode=" + predefinedFieldCode +
                ", customFieldIds=" + customFieldIds +
                '}';
    }
}
