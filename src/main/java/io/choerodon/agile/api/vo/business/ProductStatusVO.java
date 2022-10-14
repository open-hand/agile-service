package io.choerodon.agile.api.vo.business;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.Size;

import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.annotations.ApiModelProperty;

import org.hzero.starter.keyencrypt.core.Encrypt;

/**
 * @author huaxin.deng@hand-china.com
 * @since 2022/5/7
 */
@JsonInclude(JsonInclude.Include.NON_NULL)
public class ProductStatusVO {

    @Encrypt
    @ApiModelProperty("主键")
    private Long id;

    @ApiModelProperty("名称")
    @NotEmpty(message = "error.product.status.name.empty")
    @Size(max = 30, message = "error.product.status.name.size")
    private String name;

    @ApiModelProperty("来源")
    private String source;

    @ApiModelProperty("是否启用")
    private Boolean isEnabled;

    @ApiModelProperty("组织")
    private Long organizationId;

    @ApiModelProperty("乐观锁")
    private Long objectVersionNumber;

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

    public String getSource() {
        return source;
    }

    public void setSource(String source) {
        this.source = source;
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

    public Long getObjectVersionNumber() {
        return objectVersionNumber;
    }

    public void setObjectVersionNumber(Long objectVersionNumber) {
        this.objectVersionNumber = objectVersionNumber;
    }

}
