package io.choerodon.agile.api.vo;

/**
 * @author huaxin.deng@hand-china.com 2021-12-09 10:50:13
 */
public class FieldInstanceCountVO {

    private Long fieldId;

    private Integer instanceCount;

    public Long getFieldId() {
        return fieldId;
    }

    public void setFieldId(Long fieldId) {
        this.fieldId = fieldId;
    }

    public Integer getInstanceCount() {
        return instanceCount;
    }

    public void setInstanceCount(Integer instanceCount) {
        this.instanceCount = instanceCount;
    }
}
