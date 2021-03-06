package io.choerodon.agile.api.vo;

import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

import javax.validation.constraints.NotNull;

/**
 * @author shinan.chen
 * @since 2019/4/8
 */
public class PageFieldViewParamVO {
    @ApiModelProperty(value = "页面编码")
    @NotNull(message = "error.param.pageCodeNotNull")
    private String pageCode;
    @ApiModelProperty(value = "方案编码")
    @NotNull(message = "error.param.schemeCodeNotNull")
    private String schemeCode;
    @ApiModelProperty(value = "问题类型id")
    @NotNull(message = "error.param.issueTypeIdNotNull")
    @Encrypt
    private Long issueTypeId;

    @NotNull
    public String getSchemeCode() {
        return schemeCode;
    }

    public void setSchemeCode(@NotNull String schemeCode) {
        this.schemeCode = schemeCode;
    }

    public String getPageCode() {
        return pageCode;
    }

    public void setPageCode(String pageCode) {
        this.pageCode = pageCode;
    }

    public Long getIssueTypeId() {
        return issueTypeId;
    }

    public void setIssueTypeId(Long issueTypeId) {
        this.issueTypeId = issueTypeId;
    }
}
