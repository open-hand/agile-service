package io.choerodon.agile.api.vo;

import io.swagger.annotations.ApiModelProperty;

import java.util.Set;

/**
 * @author superlee
 * @since 2021-03-04
 */
public class ProjectWithUserVO {
    @ApiModelProperty(value = "项目id")
    private Long projectId;
    @ApiModelProperty(value = "用户id")
    private Set<Long> userIds;

    public Long getProjectId() {
        return projectId;
    }

    public void setProjectId(Long projectId) {
        this.projectId = projectId;
    }

    public Set<Long> getUserIds() {
        return userIds;
    }

    public void setUserIds(Set<Long> userIds) {
        this.userIds = userIds;
    }
}
