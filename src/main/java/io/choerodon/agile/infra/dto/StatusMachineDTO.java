package io.choerodon.agile.infra.dto;

import io.choerodon.mybatis.annotation.ModifyAudit;
import io.choerodon.mybatis.annotation.VersionAudit;
import io.choerodon.mybatis.domain.AuditDomain;
import org.hzero.starter.keyencrypt.core.Encrypt;

import javax.persistence.*;
import java.util.List;

/**
 * @author peng.jiang,dinghuang123@gmail.com
 */
@Table(name = "fd_status_machine")
@ModifyAudit
@VersionAudit
public class StatusMachineDTO extends AuditDomain {
    @Id
    @GeneratedValue
    @Encrypt
    private Long id;
    private String name;
    private String description;
    private String status;
    private Long organizationId;
    private Boolean isDefault;

    @Transient
    private List<StatusMachineNodeDTO> nodes;
    @Transient
    private List<StatusMachineTransformDTO> transforms;

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

    public String getStatus() {
        return status;
    }

    public void setStatus(String status) {
        this.status = status;
    }

    public Long getOrganizationId() {
        return organizationId;
    }

    public void setOrganizationId(Long organizationId) {
        this.organizationId = organizationId;
    }

    public List<StatusMachineNodeDTO> getNodes() {
        return nodes;
    }

    public void setNodes(List<StatusMachineNodeDTO> nodes) {
        this.nodes = nodes;
    }

    public List<StatusMachineTransformDTO> getTransforms() {
        return transforms;
    }

    public void setTransforms(List<StatusMachineTransformDTO> transforms) {
        this.transforms = transforms;
    }

    public Boolean getDefault() {
        return isDefault;
    }

    public void setDefault(Boolean aDefault) {
        isDefault = aDefault;
    }
}
