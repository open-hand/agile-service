package io.choerodon.agile.api.vo;

import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

import javax.validation.constraints.NotEmpty;
import java.util.List;
import java.util.Set;

/**
 * @author superlee
 * @since 2021-07-20
 */
public class FieldPermissionVO {
    @NotEmpty(message = "error.fields.empty")
    private List<ObjectSchemeFieldVO> fields;

    @Encrypt
    @NotEmpty(message = "error.issueTypeIds.empty")
    private Set<Long> issueTypeIds;
    @ApiModelProperty(value = "权限")
    private List<PermissionVO> permissions;

    public Set<Long> getIssueTypeIds() {
        return issueTypeIds;
    }

    public void setIssueTypeIds(Set<Long> issueTypeIds) {
        this.issueTypeIds = issueTypeIds;
    }

    public List<ObjectSchemeFieldVO> getFields() {
        return fields;
    }

    public void setFields(List<ObjectSchemeFieldVO> fields) {
        this.fields = fields;
    }

    public List<PermissionVO> getPermissions() {
        return permissions;
    }

    public void setPermissions(List<PermissionVO> permissions) {
        this.permissions = permissions;
    }
}
