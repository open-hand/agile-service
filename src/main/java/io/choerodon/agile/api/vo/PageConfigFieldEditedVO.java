package io.choerodon.agile.api.vo;

import io.swagger.annotations.ApiModelProperty;

/**
 * @author superlee
 * @since 2020-08-19
 */
public class PageConfigFieldEditedVO {
    @ApiModelProperty(value = "必填字段是否可编辑")
    private Boolean requiredFieldCanNotEdit;
    @ApiModelProperty(value = "创建页面字段是否可编辑")
    private Boolean createdFieldCanNotEdit;
    @ApiModelProperty(value = "编辑页面字段是否可编辑")
    private Boolean editedFieldCanNotEdit;

    public PageConfigFieldEditedVO() {
    }

    public PageConfigFieldEditedVO(Boolean requiredFieldCanNotEdit,
                                   Boolean createdFieldCanNotEdit,
                                   Boolean editedFieldCanNotEdit) {
        this.requiredFieldCanNotEdit = requiredFieldCanNotEdit;
        this.createdFieldCanNotEdit = createdFieldCanNotEdit;
        this.editedFieldCanNotEdit = editedFieldCanNotEdit;
    }

    public Boolean getRequiredFieldCanNotEdit() {
        return requiredFieldCanNotEdit;
    }

    public void setRequiredFieldCanNotEdit(Boolean requiredFieldCanNotEdit) {
        this.requiredFieldCanNotEdit = requiredFieldCanNotEdit;
    }

    public Boolean getCreatedFieldCanNotEdit() {
        return createdFieldCanNotEdit;
    }

    public void setCreatedFieldCanNotEdit(Boolean createdFieldCanNotEdit) {
        this.createdFieldCanNotEdit = createdFieldCanNotEdit;
    }

    public Boolean getEditedFieldCanNotEdit() {
        return editedFieldCanNotEdit;
    }

    public void setEditedFieldCanNotEdit(Boolean editedFieldCanNotEdit) {
        this.editedFieldCanNotEdit = editedFieldCanNotEdit;
    }
}
