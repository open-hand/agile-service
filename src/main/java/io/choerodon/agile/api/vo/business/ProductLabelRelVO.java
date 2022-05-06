package io.choerodon.agile.api.vo.business;

import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.Size;

/**
 * @author huaxin.deng@hand-china.com
 * @since 2022/5/6
 */
public class ProductLabelRelVO {

    @ApiModelProperty(value = "标签id")
    @Encrypt
    private Long labelId;

    @ApiModelProperty(value = "产品id")
    @Encrypt
    private Long productId;

    @ApiModelProperty(value = "标签名称")
    @NotEmpty(message = "error.label.name.empty")
    @Size(max = 32, message = "error.label.name.size")
    private String labelName;

    @ApiModelProperty(value = "组织id")
    private Long organizationId;

    public Long getLabelId() {
        return labelId;
    }

    public void setLabelId(Long labelId) {
        this.labelId = labelId;
    }

    public Long getProductId() {
        return productId;
    }

    public void setProductId(Long productId) {
        this.productId = productId;
    }

    public String getLabelName() {
        return labelName;
    }

    public void setLabelName(String labelName) {
        this.labelName = labelName;
    }

    public Long getOrganizationId() {
        return organizationId;
    }

    public void setOrganizationId(Long organizationId) {
        this.organizationId = organizationId;
    }
}
