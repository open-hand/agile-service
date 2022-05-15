package io.choerodon.agile.api.vo.business;

import io.choerodon.agile.api.vo.ProjectVO;
import io.choerodon.agile.infra.dto.UserMessageDTO;
import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

import javax.validation.Valid;
import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Pattern;
import javax.validation.constraints.Size;
import java.util.Date;
import java.util.List;

/**
 * @author huaxin.deng@hand-china.com
 * @since 2022/4/25
 */
public class ProductVO {

    private static final String CODE_REGULAR_EXPRESSION = "[^\\u4e00-\\u9fa5]*$";

    @Encrypt
    @ApiModelProperty(value = "产品id")
    private Long id;

    @Encrypt
    @ApiModelProperty(value = "issueId")
    private Long issueId;
    
    @ApiModelProperty(value = "图标url")
    private String imageUrl;

    @ApiModelProperty(value = "产品名称")
    @NotEmpty(message = "error.product.name.empty")
    private String name;

    @ApiModelProperty(value = "产品编码")
    @NotEmpty(message = "error.product.code.empty")
    @Size(max = 32, message = "error.product.code.size")
    @Pattern(regexp = CODE_REGULAR_EXPRESSION, message = "error.project.code.illegal")
    private String code;

    @ApiModelProperty(value = "描述")
    private String description;

    @Encrypt
    @ApiModelProperty(value = "产品负责人id")
    private Long productOwner;

    @ApiModelProperty(value = "产品负责人")
    private UserMessageDTO productOwnerUser;

    @ApiModelProperty(value = "开始时间")
    private Date startDate;

    @ApiModelProperty(value = "结束时间")
    private Date endDate;

    @ApiModelProperty(value = "是否启用")
    private Boolean isEnabled;

    @ApiModelProperty(value = "组织id")
    private Long organizationId;

    @ApiModelProperty(value = "关联项目id")
    private List<Long> projectIds;

    @ApiModelProperty(value = "关联项目")
    private List<ProjectVO> projectVOList;

    @ApiModelProperty(value = "乐观锁")
    private Long objectVersionNumber;

    @Encrypt
    @ApiModelProperty(value = "产品标签ids")
    private List<Long> productLabelIds;

    @ApiModelProperty(value = "产品标签ids")
    @Valid
    private List<ProductLabelRelVO> productLabelRelVOList;

    @Encrypt
    @ApiModelProperty(value = "产品状态id")
    @NotNull(message = "error.product.statusId.notNull")
    private Long statusId;
    @ApiModelProperty(value = "产品状态")
    private ProductStatusVO productStatusVO;

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public Long getIssueId() {
        return issueId;
    }

    public void setIssueId(Long issueId) {
        this.issueId = issueId;
    }

    public String getImageUrl() {
        return imageUrl;
    }

    public void setImageUrl(String imageUrl) {
        this.imageUrl = imageUrl;
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

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public Long getProductOwner() {
        return productOwner;
    }

    public void setProductOwner(Long productOwner) {
        this.productOwner = productOwner;
    }

    public UserMessageDTO getProductOwnerUser() {
        return productOwnerUser;
    }

    public void setProductOwnerUser(UserMessageDTO productOwnerUser) {
        this.productOwnerUser = productOwnerUser;
    }

    public Date getStartDate() {
        return startDate;
    }

    public void setStartDate(Date startDate) {
        this.startDate = startDate;
    }

    public Date getEndDate() {
        return endDate;
    }

    public void setEndDate(Date endDate) {
        this.endDate = endDate;
    }

    public Boolean getEnabled() {
        return isEnabled;
    }

    public void setEnabled(Boolean enabled) {
        isEnabled = enabled;
    }

    public Long getOrganizationId() {
        return organizationId;
    }

    public void setOrganizationId(Long organizationId) {
        this.organizationId = organizationId;
    }

    public List<Long> getProjectIds() {
        return projectIds;
    }

    public void setProjectIds(List<Long> projectIds) {
        this.projectIds = projectIds;
    }

    public List<ProjectVO> getProjectVOList() {
        return projectVOList;
    }

    public void setProjectVOList(List<ProjectVO> projectVOList) {
        this.projectVOList = projectVOList;
    }

    public Long getObjectVersionNumber() {
        return objectVersionNumber;
    }

    public void setObjectVersionNumber(Long objectVersionNumber) {
        this.objectVersionNumber = objectVersionNumber;
    }

    public List<Long> getProductLabelIds() {
        return productLabelIds;
    }

    public void setProductLabelIds(List<Long> productLabelIds) {
        this.productLabelIds = productLabelIds;
    }

    public List<ProductLabelRelVO> getProductLabelRelVOList() {
        return productLabelRelVOList;
    }

    public void setProductLabelRelVOList(List<ProductLabelRelVO> productLabelRelVOList) {
        this.productLabelRelVOList = productLabelRelVOList;
    }

    public Long getStatusId() {
        return statusId;
    }

    public void setStatusId(Long statusId) {
        this.statusId = statusId;
    }

    public ProductStatusVO getProductStatusVO() {
        return productStatusVO;
    }

    public void setProductStatusVO(ProductStatusVO productStatusVO) {
        this.productStatusVO = productStatusVO;
    }
}
