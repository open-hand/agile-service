package io.choerodon.agile.api.vo.business;

import io.swagger.annotations.ApiModelProperty;

import org.hzero.starter.keyencrypt.core.Encrypt;


public class WorkHoursConfigVO {

    @Encrypt
    @ApiModelProperty(value = "id")
    private Long id;

    @ApiModelProperty(value = "组织Id")
    private Long organizationId;

    @ApiModelProperty(value = "项目状态Id")
    @Encrypt
    private Long projectStatusId;

    @ApiModelProperty(value = "是否允许登记工时")
    private Boolean disabledAddFlag;

    @ApiModelProperty(value = "是否允许删除工时")
    private Boolean disabledDeleteFlag;
    @ApiModelProperty(value = "状态")
    private ProjectStatusVO projectStatusVO;


    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public Long getProjectStatusId() {
        return projectStatusId;
    }

    public void setProjectStatusId(Long projectStatusId) {
        this.projectStatusId = projectStatusId;
    }

    public ProjectStatusVO getProjectStatusVO() {
        return projectStatusVO;
    }

    public void setProjectStatusVO(ProjectStatusVO projectStatusVO) {
        this.projectStatusVO = projectStatusVO;
    }

    public Long getOrganizationId() {
        return organizationId;
    }

    public void setOrganizationId(Long organizationId) {
        this.organizationId = organizationId;
    }


    public Boolean getDisabledAddFlag() {
        return disabledAddFlag;
    }

    public void setDisabledAddFlag(Boolean disabledAddFlag) {
        this.disabledAddFlag = disabledAddFlag;
    }

    public Boolean getDisabledDeleteFlag() {
        return disabledDeleteFlag;
    }

    public void setDisabledDeleteFlag(Boolean disabledDeleteFlag) {
        this.disabledDeleteFlag = disabledDeleteFlag;
    }
}
