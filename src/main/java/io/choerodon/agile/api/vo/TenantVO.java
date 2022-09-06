package io.choerodon.agile.api.vo;

import io.swagger.annotations.ApiModelProperty;

/**
 * @author scp
 * @since 2020/4/21
 */
public class TenantVO {

    @ApiModelProperty(value = "组织id")
    private Long tenantId;

    @ApiModelProperty(value = "组织名称")
    private String tenantName;

    @ApiModelProperty(value = "组织编号")
    private String tenantNum;

    @ApiModelProperty(value = "是否启用")
    private Integer enabledFlag;

    public Long getTenantId() {
        return tenantId;
    }

    public void setTenantId(Long tenantId) {
        this.tenantId = tenantId;
    }

    public String getTenantName() {
        return tenantName;
    }

    public void setTenantName(String tenantName) {
        this.tenantName = tenantName;
    }

    public String getTenantNum() {
        return tenantNum;
    }

    public void setTenantNum(String tenantNum) {
        this.tenantNum = tenantNum;
    }

    public Integer getEnabledFlag() {
        return enabledFlag;
    }

    public void setEnabledFlag(Integer enabledFlag) {
        this.enabledFlag = enabledFlag;
    }
}
