package io.choerodon.agile.api.vo.business;

import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

/**
 * @author huaxin.deng@hand-china.com
 * @since 2022/4/12
 */
public class RiskInfluenceVO {

    @ApiModelProperty(value = "主键id")
    @Encrypt
    private Long id;

    @ApiModelProperty(value = "名称")
    private String name;

    @ApiModelProperty(value = "描述")
    private String description;

    @ApiModelProperty(value = "颜色")
    private String color;

    @ApiModelProperty(value = "项目id")
    private Long projectId;

    @ApiModelProperty(value = "组织id")
    private Long organizationId;

    @ApiModelProperty(value = "是否默认")
    private Boolean isDefault;

    @ApiModelProperty(value = "是否启用")
    private Boolean isEnabled;

    @ApiModelProperty(value = "排序")
    private String rank;

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

    public String getColor() {
        return color;
    }

    public void setColor(String color) {
        this.color = color;
    }

    public Long getProjectId() {
        return projectId;
    }

    public void setProjectId(Long projectId) {
        this.projectId = projectId;
    }

    public Long getOrganizationId() {
        return organizationId;
    }

    public void setOrganizationId(Long organizationId) {
        this.organizationId = organizationId;
    }

    public Boolean getDefault() {
        return isDefault;
    }

    public void setDefault(Boolean aDefault) {
        isDefault = aDefault;
    }

    public Boolean getEnabled() {
        return isEnabled;
    }

    public void setEnabled(Boolean enabled) {
        isEnabled = enabled;
    }

    public String getRank() {
        return rank;
    }

    public void setRank(String rank) {
        this.rank = rank;
    }

    @Override
    public String toString() {
        return "RiskInfluenceVO{" +
                "id=" + id +
                ", name='" + name + '\'' +
                ", description='" + description + '\'' +
                ", color='" + color + '\'' +
                ", projectId=" + projectId +
                ", organizationId=" + organizationId +
                ", isDefault=" + isDefault +
                ", isEnabled=" + isEnabled +
                ", rank='" + rank + '\'' +
                '}';
    }
}
