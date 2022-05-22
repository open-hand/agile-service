package io.choerodon.agile.api.vo;

import io.choerodon.agile.infra.feign.vo.ProjectCategoryDTO;
import io.choerodon.agile.infra.utils.StringUtil;

import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

import java.util.Date;
import java.util.List;

/**
 * @author dinghuang123@gmail.com
 * @since 2018/5/30
 */
public class ProjectVO {

    @ApiModelProperty(value = "项目主键id")
    private Long id;
    @ApiModelProperty(value = "项目名称")
    private String name;
    @ApiModelProperty(value = "组织id")
    private Long organizationId;
    @ApiModelProperty(value = "项目code")
    private String code;
    @ApiModelProperty(value = "项目是否启用")
    private Boolean enabled;
    @ApiModelProperty(value = "项目类型(非开源，一对多)")
    private List<ProjectCategoryDTO> categories;
    @ApiModelProperty(value = "版本号")
    private Long objectVersionNumber;
    @ApiModelProperty(value = "版本号")
    private String category;
    @ApiModelProperty("创建人")
    @Encrypt
    private Long createdBy;
    @ApiModelProperty(value = "图像url")
    private String imageUrl;

    @ApiModelProperty("创建时间")
    private Date creationDate;
    @ApiModelProperty(value = "项目状态")
    private String projectStatus;

    @ApiModelProperty("项目状态的名称")
    private String statusName;

    @ApiModelProperty(value = "描述")
    private String description;

    @ApiModelProperty(value = "项目类型")
    private List<String> types;

    public String getStatusName() {
        return statusName;
    }

    public void setStatusName(String statusName) {
        this.statusName = statusName;
    }

    public String getProjectStatus() {
        return projectStatus;
    }

    public void setProjectStatus(String projectStatus) {
        this.projectStatus = projectStatus;
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

    public Long getOrganizationId() {
        return organizationId;
    }

    public void setOrganizationId(Long organizationId) {
        this.organizationId = organizationId;
    }

    public String getCode() {
        return code;
    }

    public void setCode(String code) {
        this.code = code;
    }

    public Boolean getEnabled() {
        return enabled;
    }

    public void setEnabled(Boolean enabled) {
        this.enabled = enabled;
    }

    public List<ProjectCategoryDTO> getCategories() {
        return categories;
    }

    public void setCategories(List<ProjectCategoryDTO> categories) {
        this.categories = categories;
    }

    public Long getObjectVersionNumber() {
        return objectVersionNumber;
    }

    public void setObjectVersionNumber(Long objectVersionNumber) {
        this.objectVersionNumber = objectVersionNumber;
    }

    public String getCategory() {
        return category;
    }

    public void setCategory(String category) {
        this.category = category;
    }

    public Long getCreatedBy() {
        return createdBy;
    }

    public void setCreatedBy(Long createdBy) {
        this.createdBy = createdBy;
    }

    @Override
    public String toString() {
        return StringUtil.getToString(this);
    }

    public String getImageUrl() {
        return imageUrl;
    }

    public void setImageUrl(String imageUrl) {
        this.imageUrl = imageUrl;
    }

    public Date getCreationDate() {
        return creationDate;
    }

    public void setCreationDate(Date creationDate) {
        this.creationDate = creationDate;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public List<String> getTypes() {
        return types;
    }

    public void setTypes(List<String> types) {
        this.types = types;
    }
}
