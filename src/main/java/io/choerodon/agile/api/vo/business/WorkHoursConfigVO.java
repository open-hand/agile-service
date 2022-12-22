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
    private Boolean canAddFlag;

    @ApiModelProperty(value = "是否允许删除工时")
    private Boolean canDeleteFlag;
    @ApiModelProperty(value = "状态")
    private ProjectStatusVO iamProjectStatusVO;


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

    public ProjectStatusVO getIamProjectStatusVO() {
        return iamProjectStatusVO;
    }

    public void setIamProjectStatusVO(ProjectStatusVO iamProjectStatusVO) {
        this.iamProjectStatusVO = iamProjectStatusVO;
    }

    public Long getOrganizationId() {
        return organizationId;
    }

    public void setOrganizationId(Long organizationId) {
        this.organizationId = organizationId;
    }

    public Boolean getCanAddFlag() {
        return canAddFlag;
    }

    public void setCanAddFlag(Boolean canAddFlag) {
        this.canAddFlag = canAddFlag;
    }

    public Boolean getCanDeleteFlag() {
        return canDeleteFlag;
    }

    public void setCanDeleteFlag(Boolean canDeleteFlag) {
        this.canDeleteFlag = canDeleteFlag;
    }
}
