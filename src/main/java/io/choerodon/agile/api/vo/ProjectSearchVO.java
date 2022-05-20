package io.choerodon.agile.api.vo;

import io.swagger.annotations.ApiModelProperty;

import java.util.List;

/**
 * @author superlee
 * @since 2021-01-21
 */
public class ProjectSearchVO {
    @ApiModelProperty(value = "类别id集合")
    private List<Long> categoryIds;
    @ApiModelProperty(value = "是否启用")
    private Boolean enable;
    @ApiModelProperty(value = "类别编码")
    private List<String> categoryCodes;

    public List<Long> getCategoryIds() {
        return categoryIds;
    }

    public void setCategoryIds(List<Long> categoryIds) {
        this.categoryIds = categoryIds;
    }

    public Boolean getEnable() {
        return enable;
    }

    public void setEnable(Boolean enable) {
        this.enable = enable;
    }

    public List<String> getCategoryCodes() {
        return categoryCodes;
    }

    public void setCategoryCodes(List<String> categoryCodes) {
        this.categoryCodes = categoryCodes;
    }
}
