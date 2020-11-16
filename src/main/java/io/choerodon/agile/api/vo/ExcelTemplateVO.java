package io.choerodon.agile.api.vo;

import java.util.List;

/**
 * @author superlee
 * @since 2020-11-11
 */
public class ExcelTemplateVO {

    private List<String> systemFields;

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
