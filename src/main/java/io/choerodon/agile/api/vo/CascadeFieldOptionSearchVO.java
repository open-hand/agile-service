package io.choerodon.agile.api.vo;

import org.hzero.starter.keyencrypt.core.Encrypt;

import java.util.List;

/**
 * @author chihao.ran@hand-china.com
 * 2021/07/16 9:18
 */
public class CascadeFieldOptionSearchVO {
    @Encrypt
    private List<Long> fieldCascadeRuleIds;
    @Encrypt
    private List<Long> selected;
    private String searchParam;
    private List<String> extendParams;

    public List<Long> getFieldCascadeRuleIds() {
        return fieldCascadeRuleIds;
    }

    public void setFieldCascadeRuleIds(List<Long> fieldCascadeRuleIds) {
        this.fieldCascadeRuleIds = fieldCascadeRuleIds;
    }

    public List<Long> getSelected() {
        return selected;
    }

    public void setSelected(List<Long> selected) {
        this.selected = selected;
    }

    public String getSearchParam() {
        return searchParam;
    }

    public void setSearchParam(String searchParam) {
        this.searchParam = searchParam;
    }

    public List<String> getExtendParams() {
        return extendParams;
    }

    public void setExtendParams(List<String> extendParams) {
        this.extendParams = extendParams;
    }
}
