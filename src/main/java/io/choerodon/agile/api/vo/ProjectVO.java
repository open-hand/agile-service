package io.choerodon.agile.api.vo;

import java.util.Date;
import java.util.List;
import java.util.Set;

import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.annotations.ApiModelProperty;

import io.choerodon.agile.api.vo.business.ArtVO;
import io.choerodon.agile.infra.feign.vo.ProjectCategoryDTO;
import io.choerodon.agile.infra.utils.StringUtil;

import org.hzero.starter.keyencrypt.core.Encrypt;

/**
 * @author dinghuang123@gmail.com
 * @since 2018/5/30
 */
@JsonInclude(JsonInclude.Include.NON_NULL)
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
    @ApiModelProperty(value = "项目id集合")
    private Set<Long> topProjectIds;
    @ApiModelProperty(value = "模糊搜索参数")
    private String param;
    @ApiModelProperty("项目群ART信息")
    private ArtVO artInfo;

    public String getParam() {
        return param;
    }

    public ProjectVO setParam(String param) {
        this.param = param;
        return this;
    }

    public Set<Long> getTopProjectIds() {
        return topProjectIds;
    }

    public ProjectVO setTopProjectIds(Set<Long> topProjectIds) {
        this.topProjectIds = topProjectIds;
        return this;
    }

    public String getStatusName() {
        return statusName;
    }

    public ProjectVO setStatusName(String statusName) {
        this.statusName = statusName;
        return this;
    }

    public String getProjectStatus() {
        return projectStatus;
    }

    public ProjectVO setProjectStatus(String projectStatus) {
        this.projectStatus = projectStatus;
        return this;
    }

    public Long getId() {
        return id;
    }

    public ProjectVO setId(Long id) {
        this.id = id;
        return this;
    }

    public String getName() {
        return name;
    }

    public ProjectVO setName(String name) {
        this.name = name;
        return this;
    }

    public Long getOrganizationId() {
        return organizationId;
    }

    public ProjectVO setOrganizationId(Long organizationId) {
        this.organizationId = organizationId;
        return this;
    }

    public String getCode() {
        return code;
    }

    public ProjectVO setCode(String code) {
        this.code = code;
        return this;
    }

    public Boolean getEnabled() {
        return enabled;
    }

    public ProjectVO setEnabled(Boolean enabled) {
        this.enabled = enabled;
        return this;
    }

    public List<ProjectCategoryDTO> getCategories() {
        return categories;
    }

    public ProjectVO setCategories(List<ProjectCategoryDTO> categories) {
        this.categories = categories;
        return this;
    }

    public Long getObjectVersionNumber() {
        return objectVersionNumber;
    }

    public ProjectVO setObjectVersionNumber(Long objectVersionNumber) {
        this.objectVersionNumber = objectVersionNumber;
        return this;
    }

    public String getCategory() {
        return category;
    }

    public ProjectVO setCategory(String category) {
        this.category = category;
        return this;
    }

    public Long getCreatedBy() {
        return createdBy;
    }

    public ProjectVO setCreatedBy(Long createdBy) {
        this.createdBy = createdBy;
        return this;
    }

    @Override
    public String toString() {
        return StringUtil.getToString(this);
    }

    public String getImageUrl() {
        return imageUrl;
    }

    public ProjectVO setImageUrl(String imageUrl) {
        this.imageUrl = imageUrl;
        return this;
    }

    public Date getCreationDate() {
        return creationDate;
    }

    public ProjectVO setCreationDate(Date creationDate) {
        this.creationDate = creationDate;
        return this;
    }

    public String getDescription() {
        return description;
    }

    public ProjectVO setDescription(String description) {
        this.description = description;
        return this;
    }

    public List<String> getTypes() {
        return types;
    }

    public ProjectVO setTypes(List<String> types) {
        this.types = types;
        return this;
    }
    /**
     * @return 项目群ART信息
     */
    public ArtVO getArtInfo() {
        return artInfo;
    }

    public ProjectVO setArtInfo(ArtVO artInfo) {
        this.artInfo = artInfo;
        return this;
    }
}
