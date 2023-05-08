package io.choerodon.agile.infra.dto;

import io.choerodon.mybatis.annotation.ModifyAudit;
import io.choerodon.mybatis.annotation.VersionAudit;
import io.choerodon.mybatis.domain.AuditDomain;

import javax.persistence.*;

/**
 * @author peng.jiang,dinghuang123@gmail.com
 */
@Table(name = "fd_status_machine_transform")
@ModifyAudit
@VersionAudit
public class StatusMachineTransformDTO extends AuditDomain {
    @Id
    @GeneratedValue
    private Long id;
    private String name;
    private String description;
    private Long stateMachineId;
    private Long startNodeId;
    private Long endNodeId;
    /**
     * 页面方案id
     */
    private String url;
    private String type;
    private String style;
    private String conditionStrategy;
    private Long organizationId;

    @Transient
    private Long endStatusId;

    public StatusMachineTransformDTO() {
    }

    public StatusMachineTransformDTO(String name, Long stateMachineId, Long startNodeId, Long endNodeId, String type, String conditionStrategy, Long organizationId) {
        this.name = name;
        this.stateMachineId = stateMachineId;
        this.startNodeId = startNodeId;
        this.endNodeId = endNodeId;
        this.type = type;
        this.conditionStrategy = conditionStrategy;
        this.organizationId = organizationId;
    }

    public Long getEndStatusId() {
        return endStatusId;
    }

    public void setEndStatusId(Long endStatusId) {
        this.endStatusId = endStatusId;
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

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public Long getStartNodeId() {
        return startNodeId;
    }

    public void setStartNodeId(Long startNodeId) {
        this.startNodeId = startNodeId;
    }

    public Long getEndNodeId() {
        return endNodeId;
    }

    public void setEndNodeId(Long endNodeId) {
        this.endNodeId = endNodeId;
    }

    public String getUrl() {
        return url;
    }

    public void setUrl(String url) {
        this.url = url;
    }

    public Long getStateMachineId() {
        return stateMachineId;
    }

    public StatusMachineTransformDTO setStateMachineId(Long stateMachineId) {
        this.stateMachineId = stateMachineId;
        return this;
    }

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    public String getStyle() {
        return style;
    }

    public void setStyle(String style) {
        this.style = style;
    }

    public String getConditionStrategy() {
        return conditionStrategy;
    }

    public void setConditionStrategy(String conditionStrategy) {
        this.conditionStrategy = conditionStrategy;
    }

    public Long getOrganizationId() {
        return organizationId;
    }

    public void setOrganizationId(Long organizationId) {
        this.organizationId = organizationId;
    }
}
