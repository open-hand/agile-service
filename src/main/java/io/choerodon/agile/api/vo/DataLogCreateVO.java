package io.choerodon.agile.api.vo;

import io.choerodon.agile.infra.constants.EncryptionConstant;
import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

/**
 * @author shinan.chen
 * @since 2019/6/11
 */
public class DataLogCreateVO {
    @ApiModelProperty(value = "问题主键id")
    @Encrypt/*(EncryptionConstant.AGILE_ISSUE)*/
    private Long issueId;
    @ApiModelProperty(value = "问题字段")
    private String field;
    @ApiModelProperty(value = "旧值")
    private String oldValue;
    @ApiModelProperty(value = "旧值str")
    private String oldString;
    @ApiModelProperty(value = "新值")
    private String newValue;
    @ApiModelProperty(value = "新值str")
    private String newString;

    public String getField() {
        return field;
    }

    public void setField(String field) {
        this.field = field;
    }

    public String getOldValue() {
        return oldValue;
    }

    public void setOldValue(String oldValue) {
        this.oldValue = oldValue;
    }

    public String getOldString() {
        return oldString;
    }

    public void setOldString(String oldString) {
        this.oldString = oldString;
    }

    public String getNewValue() {
        return newValue;
    }

    public void setNewValue(String newValue) {
        this.newValue = newValue;
    }

    public String getNewString() {
        return newString;
    }

    public void setNewString(String newString) {
        this.newString = newString;
    }

    public Long getIssueId() {
        return issueId;
    }

    public void setIssueId(Long issueId) {
        this.issueId = issueId;
    }
}
