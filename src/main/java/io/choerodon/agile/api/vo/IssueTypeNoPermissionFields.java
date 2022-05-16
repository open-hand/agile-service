package io.choerodon.agile.api.vo;

import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

import java.util.List;

/**
 * @author huaxin.deng@hand-china.com
 * @since 2022/3/24
 */
public class IssueTypeNoPermissionFields {

    @Encrypt
    @ApiModelProperty(value = "问题类型id")
    private Long issueTypeId;
    @ApiModelProperty(value = "问题类型名称")
    private String issueTypeName;
    @ApiModelProperty(value = "问题类型编码")
    private String typeCode;
    @ApiModelProperty(value = "没有权限的字段编码")
    private List<String> noPermissionFieldCodes;

    public Long getIssueTypeId() {
        return issueTypeId;
    }

    public void setIssueTypeId(Long issueTypeId) {
        this.issueTypeId = issueTypeId;
    }

    public String getIssueTypeName() {
        return issueTypeName;
    }

    public void setIssueTypeName(String issueTypeName) {
        this.issueTypeName = issueTypeName;
    }

    public String getTypeCode() {
        return typeCode;
    }

    public void setTypeCode(String typeCode) {
        this.typeCode = typeCode;
    }

    public List<String> getNoPermissionFieldCodes() {
        return noPermissionFieldCodes;
    }

    public void setNoPermissionFieldCodes(List<String> noPermissionFieldCodes) {
        this.noPermissionFieldCodes = noPermissionFieldCodes;
    }
}
