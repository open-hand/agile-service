package io.choerodon.agile.api.vo.business;

/**
 * @author superlee
 * @since 2022-02-16
 */
public class SystemFieldOverrideConfigVO {

    private String fieldCode;

    private String issueTypeCode;

    private Boolean required;

    private Boolean edited;

    private Boolean created;

    public String getFieldCode() {
        return fieldCode;
    }

    public void setFieldCode(String fieldCode) {
        this.fieldCode = fieldCode;
    }

    public String getIssueTypeCode() {
        return issueTypeCode;
    }

    public void setIssueTypeCode(String issueTypeCode) {
        this.issueTypeCode = issueTypeCode;
    }

    public Boolean getRequired() {
        return required;
    }

    public void setRequired(Boolean required) {
        this.required = required;
    }

    public Boolean getEdited() {
        return edited;
    }

    public void setEdited(Boolean edited) {
        this.edited = edited;
    }

    public Boolean getCreated() {
        return created;
    }

    public void setCreated(Boolean created) {
        this.created = created;
    }
}
