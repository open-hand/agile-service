package io.choerodon.agile.api.vo;

/**
 * @author superlee
 * @since 2020-08-19
 */
public class PageConfigFieldEditedVO {

    private Boolean requiredFieldCanNotEdit;

    private Boolean createdFieldCanNotEdit;

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
