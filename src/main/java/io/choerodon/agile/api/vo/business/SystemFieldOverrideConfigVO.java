package io.choerodon.agile.api.vo.business;

import io.swagger.annotations.ApiModelProperty;

/**
 * @author superlee
 * @since 2022-02-16
 */
public class SystemFieldOverrideConfigVO {
    @ApiModelProperty(value = "字段编码")
    private String fieldCode;
    @ApiModelProperty(value = "问题类型编码")
    private String issueTypeCode;
    @ApiModelProperty(value = "是否为必填")
    private Boolean required;
    @ApiModelProperty(value = "是否在编辑页面")
    private Boolean edited;
    @ApiModelProperty(value = "是否在创建页面")
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
