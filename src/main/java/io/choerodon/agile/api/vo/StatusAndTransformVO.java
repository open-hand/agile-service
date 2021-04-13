package io.choerodon.agile.api.vo;

import org.hzero.starter.keyencrypt.core.Encrypt;

import java.util.Set;

/**
 * @author zhaotianxin
 * @date 2020-08-05 16:04
 */
public class StatusAndTransformVO {
    @Encrypt
    private Long id;

    private String name;

    private String code;

    private String type;
    @Encrypt
    private Long nodeId;
    @Encrypt
    private Set<Long> canTransformStatus;
    @Encrypt
    private Long stateMachineId;

    private Boolean defaultStatus;

    private Boolean hasIssue;

    private Boolean completed;

    public Boolean getCompleted() {
        return completed;
    }

    public void setCompleted(Boolean completed) {
        this.completed = completed;
    }

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

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    public Set<Long> getCanTransformStatus() {
        return canTransformStatus;
    }

    public void setCanTransformStatus(Set<Long> canTransformStatus) {
        this.canTransformStatus = canTransformStatus;
    }

    public Long getStateMachineId() {
        return stateMachineId;
    }

    public void setStateMachineId(Long stateMachineId) {
        this.stateMachineId = stateMachineId;
    }

    public Boolean getDefaultStatus() {
        return defaultStatus;
    }

    public void setDefaultStatus(Boolean defaultStatus) {
        this.defaultStatus = defaultStatus;
    }

    public Long getNodeId() {
        return nodeId;
    }

    public void setNodeId(Long nodeId) {
        this.nodeId = nodeId;
    }

    public Boolean getHasIssue() {
        return hasIssue;
    }

    public void setHasIssue(Boolean hasIssue) {
        this.hasIssue = hasIssue;
    }
}
