package io.choerodon.agile.api.vo;

import java.util.List;

import io.swagger.annotations.ApiModelProperty;

/**
 * @author superlee
 * @since 2021-01-21
 */
public class ProjectSearchVO {
    @ApiModelProperty(value = "类别id集合")
    private List<Long> categoryIds;
    @ApiModelProperty(value = "是否启用")
    private Boolean enable;
    @ApiModelProperty(value = "是否为模板")
    private Boolean templateFlag;
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

    /**
     * @return 是否为模板
     */
    public Boolean getTemplateFlag() {
        return templateFlag;
    }

    public ProjectSearchVO setTemplateFlag(Boolean templateFlag) {
        this.templateFlag = templateFlag;
        return this;
    }

    public List<String> getCategoryCodes() {
        return categoryCodes;
    }

    public void setCategoryCodes(List<String> categoryCodes) {
        this.categoryCodes = categoryCodes;
    }

}
