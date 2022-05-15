package io.choerodon.agile.api.vo;

import io.swagger.annotations.ApiModelProperty;

/**
 * @author superlee
 * @since 2021-07-08
 */
public class BranchVO {
    @ApiModelProperty("分支id")
    private Long branchId;
    @ApiModelProperty("项目id")
    private Long projectId;
    @ApiModelProperty("应用服务id")
    private Long appServiceId;
    @ApiModelProperty("应用服务编码")
    private String appServiceCode;

    public Long getBranchId() {
        return branchId;
    }

    public void setBranchId(Long branchId) {
        this.branchId = branchId;
    }

    public Long getProjectId() {
        return projectId;
    }

    public void setProjectId(Long projectId) {
        this.projectId = projectId;
    }

    public Long getAppServiceId() {
        return appServiceId;
    }

    public void setAppServiceId(Long appServiceId) {
        this.appServiceId = appServiceId;
    }

    public String getAppServiceCode() {
        return appServiceCode;
    }

    public void setAppServiceCode(String appServiceCode) {
        this.appServiceCode = appServiceCode;
    }
}
