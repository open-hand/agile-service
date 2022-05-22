package io.choerodon.agile.api.vo;

import io.choerodon.agile.infra.dto.UserMessageDTO;
import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

import java.util.List;
import java.util.Set;

/**
 * @author superlee
 * @since 2021-07-20
 */
public class PermissionVO {
    @ApiModelProperty(value = "范围")
    private String scope;
    @Encrypt
    @ApiModelProperty(value = "用户id")
    private Set<Long> userIds;
    @Encrypt
    @ApiModelProperty(value = "角色id")
    private Set<Long> roleIds;
    @ApiModelProperty(value = "用户")
    private List<UserMessageDTO> userList;
    @ApiModelProperty(value = "角色")
    private List<RoleVO> roleList;

    public List<UserMessageDTO> getUserList() {
        return userList;
    }

    public void setUserList(List<UserMessageDTO> userList) {
        this.userList = userList;
    }

    public List<RoleVO> getRoleList() {
        return roleList;
    }

    public void setRoleList(List<RoleVO> roleList) {
        this.roleList = roleList;
    }

    public String getScope() {
        return scope;
    }

    public void setScope(String scope) {
        this.scope = scope;
    }

    public Set<Long> getUserIds() {
        return userIds;
    }

    public void setUserIds(Set<Long> userIds) {
        this.userIds = userIds;
    }

    public Set<Long> getRoleIds() {
        return roleIds;
    }

    public void setRoleIds(Set<Long> roleIds) {
        this.roleIds = roleIds;
    }
}
