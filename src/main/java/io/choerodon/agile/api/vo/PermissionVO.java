package io.choerodon.agile.api.vo;

import io.choerodon.agile.infra.dto.UserMessageDTO;
import org.hzero.starter.keyencrypt.core.Encrypt;

import java.util.List;
import java.util.Set;

/**
 * @author superlee
 * @since 2021-07-20
 */
public class PermissionVO {

    private String scope;
    @Encrypt
    private Set<Long> userIds;
    @Encrypt
    private Set<Long> roleIds;

    private List<UserMessageDTO> userList;

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
