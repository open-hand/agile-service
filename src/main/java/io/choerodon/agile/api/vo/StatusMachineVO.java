package io.choerodon.agile.api.vo;

import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

import java.util.List;
import java.util.Set;

/**
 * @author peng.jiang, dinghuang123@gmail.com
 */
public class StatusMachineVO {
    @ApiModelProperty(value = "状态机id")
    @Encrypt
    private Long id;
    @ApiModelProperty(value = "名称")
    private String name;
    @ApiModelProperty(value = "描述")
    private String description;
    @ApiModelProperty(value = "状态机状态（state_machine_draft/state_machine_active/state_machine_create）")
    private String status;
    @ApiModelProperty(value = "组织id")
    private Long organizationId;
    @ApiModelProperty(value = "是否默认状态机")
    private Boolean isDefault;
    @ApiModelProperty(value = "乐观锁")
    private Long objectVersionNumber;
    @ApiModelProperty(value = "状态机节点列表")
    private List<StatusMachineNodeVO> nodeVOS;
    @ApiModelProperty(value = "状态机转换列表")
    private List<StatusMachineTransformVO> transformVOS;
    @ApiModelProperty(value = "应用类型")
    private Set<String> applyTypes;

    public Set<String> getApplyTypes() {
        return applyTypes;
    }

    public void setApplyTypes(Set<String> applyTypes) {
        this.applyTypes = applyTypes;
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

    public List<StatusMachineNodeVO> getNodeVOS() {
        return nodeVOS;
    }

    public void setNodeVOS(List<StatusMachineNodeVO> nodeVOS) {
        this.nodeVOS = nodeVOS;
    }

    public List<StatusMachineTransformVO> getTransformVOS() {
        return transformVOS;
    }

    public void setTransformVOS(List<StatusMachineTransformVO> transformVOS) {
        this.transformVOS = transformVOS;
    }

    public Long getObjectVersionNumber() {
        return objectVersionNumber;
    }

    public void setObjectVersionNumber(Long objectVersionNumber) {
        this.objectVersionNumber = objectVersionNumber;
    }

    public Boolean getDefault() {
        return isDefault;
    }

    public void setDefault(Boolean aDefault) {
        isDefault = aDefault;
    }
}
