package io.choerodon.agile.api.vo.business;

import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

/**
 * @author huaxin.deng@hand-china.com
 * @since 2022/5/6
 */
public class ProductLabelVO {

    @ApiModelProperty(value = "标签id")
    @Encrypt
    private Long id;

    @ApiModelProperty(value = "标签名称")
    private String labelName;

    @ApiModelProperty(value = "组织id")
    private Long organizationId;

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
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
