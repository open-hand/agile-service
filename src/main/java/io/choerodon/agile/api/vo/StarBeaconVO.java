package io.choerodon.agile.api.vo;

import org.hzero.starter.keyencrypt.core.Encrypt;

/**
 * @author jiaxu.cui@hand-china.com 2020/11/9 14:19
 */
public class StarBeaconVO {

    @Encrypt
    private Long instanceId;
    private Long organizationId;
    private Long projectId;
    @Encrypt
    private Long userId;
    private String type;

    public Long getUserId() {
        return userId;
    }

    public void setUserId(Long userId) {
        this.userId = userId;
    }

    public Long getInstanceId() {
        return instanceId;
    }

    public void setInstanceId(Long instanceId) {
        this.instanceId = instanceId;
    }

    public Long getOrganizationId() {
        return organizationId;
    }

    public void setOrganizationId(Long organizationId) {
        this.organizationId = organizationId;
    }

    public Long getProjectId() {
        return projectId;
    }

    public void setProjectId(Long projectId) {
        this.projectId = projectId;
    }

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }
}
