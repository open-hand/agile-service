package io.choerodon.agile.api.vo;

import org.hzero.starter.keyencrypt.core.Encrypt;

import java.util.List;

/**
 * @author huaxin.deng@hand-china.com
 * @since 2022/3/24
 */
public class IssueTypeNoPermissionFields {

    @Encrypt
    private Long issueTypeId;

    private String issueTypeName;

    private String typeCode;

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
