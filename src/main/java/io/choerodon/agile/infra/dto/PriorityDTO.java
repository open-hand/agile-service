package io.choerodon.agile.infra.dto;

import io.choerodon.mybatis.annotation.ModifyAudit;
import io.choerodon.mybatis.annotation.VersionAudit;
import io.choerodon.mybatis.domain.AuditDomain;

import javax.persistence.*;
import java.math.BigDecimal;

/**
 * @author cong.cheng
 * @Date 2018/8/21
 */
@Table(name = "fd_priority")
@ModifyAudit
@VersionAudit
public class PriorityDTO extends AuditDomain {
    @Id
    @GeneratedValue
    private Long id;

    private String name;
    private String description;
    private String colour;
    private Long organizationId;
    private Boolean isDefault;
    private BigDecimal sequence;
    @Column(name = "is_enable")
    private Boolean enable;

    public PriorityDTO() {}

    public PriorityDTO(Long organizationId, Long id) {
        this.organizationId = organizationId;
        this.id = id;
    }

    public Boolean getEnable() {
        return enable;
    }

    public void setEnable(Boolean enable) {
        this.enable = enable;
    }

    public BigDecimal getSequence() {
        return sequence;
    }

    public void setSequence(BigDecimal sequence) {
        this.sequence = sequence;
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

    public String getColour() {
        return colour;
    }

    public void setColour(String colour) {
        this.colour = colour;
    }

    public Long getOrganizationId() {
        return organizationId;
    }

    public void setOrganizationId(Long organizationId) {
        this.organizationId = organizationId;
    }

    public void setDefault(Boolean aDefault) {
        isDefault = aDefault;
    }

    public Boolean getDefault() {
        return isDefault;
    }
}
