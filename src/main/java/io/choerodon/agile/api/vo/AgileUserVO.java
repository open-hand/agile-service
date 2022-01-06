package io.choerodon.agile.api.vo;


import org.hzero.starter.keyencrypt.core.Encrypt;

import java.util.Set;

/**
 * @author superlee
 * @since 2020-10-14
 */
public class AgileUserVO {

    @Encrypt(ignoreValue = {"0"})
    private Set<Long> userIds;

    private Set<Long> projectIds;

    private String param;

    private Long organizationId;

    @Encrypt(ignoreValue = {"0"})
    private Set<Long> ignoredUserIds;

    private String loginName;

    private String realName;

    private Boolean enabled;

    private String roleName;

    private Boolean locked;

    public AgileUserVO() {}

    public AgileUserVO(Set<Long> userIds, Set<Long> projectIds, String param, Long organizationId, Set<Long> ignoredUserIds) {
        this.userIds = userIds;
        this.projectIds = projectIds;
        this.param = param;
        this.organizationId = organizationId;
        this.ignoredUserIds = ignoredUserIds;
    }

    public Long getOrganizationId() {
        return organizationId;
    }

    public void setOrganizationId(Long organizationId) {
        this.organizationId = organizationId;
    }

    public String getParam() {
        return param;
    }

    public void setParam(String param) {
        this.param = param;
    }

    public Set<Long> getUserIds() {
        return userIds;
    }

    public void setUserIds(Set<Long> userIds) {
        this.userIds = userIds;
    }

    public Set<Long> getProjectIds() {
        return projectIds;
    }

    public void setProjectIds(Set<Long> projectIds) {
        this.projectIds = projectIds;
    }

    public Set<Long> getIgnoredUserIds() {
        return ignoredUserIds;
    }

    public void setIgnoredUserIds(Set<Long> ignoredUserIds) {
        this.ignoredUserIds = ignoredUserIds;
    }

    public String getLoginName() {
        return loginName;
    }

    public void setLoginName(String loginName) {
        this.loginName = loginName;
    }

    public String getRealName() {
        return realName;
    }

    public void setRealName(String realName) {
        this.realName = realName;
    }

    public Boolean getEnabled() {
        return enabled;
    }

    public void setEnabled(Boolean enabled) {
        this.enabled = enabled;
    }

    public String getRoleName() {
        return roleName;
    }

    public void setRoleName(String roleName) {
        this.roleName = roleName;
    }

    public Boolean getLocked() {
        return locked;
    }

    public void setLocked(Boolean locked) {
        this.locked = locked;
    }
}
