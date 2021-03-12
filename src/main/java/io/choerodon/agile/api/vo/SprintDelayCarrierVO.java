package io.choerodon.agile.api.vo;

import io.choerodon.agile.infra.dto.SprintDTO;

import java.util.Set;

/**
 * @author superlee
 * @since 2021-03-03
 */
public class SprintDelayCarrierVO {

    private Long projectId;

    private SprintDTO sprintDTO;

    private Long delayDay;

    private String projectName;

    private Set<Long> userIds;

    private Long organizationId;

    public SprintDelayCarrierVO(Long projectId,
                                SprintDTO sprintDTO,
                                Long delayDay,
                                String projectName,
                                Long organizationId) {
        this.projectId = projectId;
        this.sprintDTO = sprintDTO;
        this.delayDay = delayDay;
        this.projectName = projectName;
        this.organizationId = organizationId;
    }

    public Long getOrganizationId() {
        return organizationId;
    }

    public void setOrganizationId(Long organizationId) {
        this.organizationId = organizationId;
    }

    public String getProjectName() {
        return projectName;
    }

    public void setProjectName(String projectName) {
        this.projectName = projectName;
    }

    public Set<Long> getUserIds() {
        return userIds;
    }

    public void setUserIds(Set<Long> userIds) {
        this.userIds = userIds;
    }

    public Long getProjectId() {
        return projectId;
    }

    public void setProjectId(Long projectId) {
        this.projectId = projectId;
    }

    public SprintDTO getSprintDTO() {
        return sprintDTO;
    }

    public void setSprintDTO(SprintDTO sprintDTO) {
        this.sprintDTO = sprintDTO;
    }

    public Long getDelayDay() {
        return delayDay;
    }

    public void setDelayDay(Long delayDay) {
        this.delayDay = delayDay;
    }
}
