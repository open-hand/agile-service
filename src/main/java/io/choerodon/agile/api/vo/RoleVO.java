package io.choerodon.agile.api.vo;

import io.choerodon.agile.infra.dto.UserDTO;
import io.swagger.annotations.ApiModelProperty;

import java.util.List;

/**
 * Created by HuangFuqiang@choerodon.io on 2018/10/9.
 * Email: fuqianghuang01@gmail.com
 */
public class RoleVO {

    @ApiModelProperty(value = "角色id")
    private Long id;

    @ApiModelProperty(value = "角色名/必填")
    private String name;

    @ApiModelProperty(value = "角色编码/必填")
    private String code;

    @ApiModelProperty(value = "角色描述/非必填")
    private String description;

    @ApiModelProperty(value = "角色层级/必填")
    private String resourceLevel;

    @ApiModelProperty(value = "组织ID/非必填")
    private Long organizationId;

    @ApiModelProperty(value = "是否启用/非必填")
    private Boolean enabled;

    @ApiModelProperty(value = "是否允许修改/非必填")
    private Boolean modified;

    @ApiModelProperty(value = "是否允许禁用/非必填")
    private Boolean enableForbidden;

    @ApiModelProperty(value = "是否内置角色/非必填")
    private Boolean builtIn;
    @ApiModelProperty(value = "组织名")
    private String organizationName;
    @ApiModelProperty(value = "项目名")
    private String projectName;
    @ApiModelProperty(value = "用户")
    private List<UserDTO> users;

    @ApiModelProperty(value = "用户数量")
    private Integer userCount;
    @ApiModelProperty(value = "角色id")
    private List<Long> roleIds;


    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getCode() {
        return code;
    }

    public void setCode(String code) {
        this.code = code;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public String getResourceLevel() {
        return resourceLevel;
    }

    public void setResourceLevel(String resourceLevel) {
        this.resourceLevel = resourceLevel;
    }

    public Boolean getEnabled() {
        return enabled;
    }

    public void setEnabled(Boolean enabled) {
        this.enabled = enabled;
    }

    public Boolean getModified() {
        return modified;
    }

    public void setModified(Boolean modified) {
        this.modified = modified;
    }

    public Boolean getEnableForbidden() {
        return enableForbidden;
    }

    public void setEnableForbidden(Boolean enableForbidden) {
        this.enableForbidden = enableForbidden;
    }

    public Boolean getBuiltIn() {
        return builtIn;
    }

    public void setBuiltIn(Boolean builtIn) {
        this.builtIn = builtIn;
    }

    public String getOrganizationName() {
        return organizationName;
    }

    public void setOrganizationName(String organizationName) {
        this.organizationName = organizationName;
    }

    public String getProjectName() {
        return projectName;
    }

    public void setProjectName(String projectName) {
        this.projectName = projectName;
    }

    public List<UserDTO> getUsers() {
        return users;
    }

    public void setUsers(List<UserDTO> users) {
        this.users = users;
    }

    public Integer getUserCount() {
        return userCount;
    }

    public void setUserCount(Integer userCount) {
        this.userCount = userCount;
    }

    public List<Long> getRoleIds() {
        return roleIds;
    }

    public void setRoleIds(List<Long> roleIds) {
        this.roleIds = roleIds;
    }


    public Long getOrganizationId() {
        return organizationId;
    }

    public void setOrganizationId(Long organizationId) {
        this.organizationId = organizationId;
    }


}
