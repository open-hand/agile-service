package io.choerodon.agile.api.vo.business;

import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;


public class AgileWorkHoursConfigVO {

    @Encrypt
    @ApiModelProperty(value = "id")
    private Long id;

    @ApiModelProperty(value = "组织Id")
    private Long tenantId;

    @ApiModelProperty(value = "项目状态Id")
    @Encrypt
    private Long projectStatusId;

    @ApiModelProperty(value = "是否允许登记工时")
    private Boolean register;

    @ApiModelProperty(value = "是否允许删除工时")
    private Boolean delete;

    @ApiModelProperty(value = "状态")
    private IamProjectStatusVO iamProjectStatusVO;


    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public Long getTenantId() {
        return tenantId;
    }

    public void setTenantId(Long tenantId) {
        this.tenantId = tenantId;
    }

    public Long getProjectStatusId() {
        return projectStatusId;
    }

    public void setProjectStatusId(Long projectStatusId) {
        this.projectStatusId = projectStatusId;
    }

    public Boolean getRegister() {
        return register;
    }

    public void setRegister(Boolean register) {
        this.register = register;
    }

    public Boolean getDelete() {
        return delete;
    }

    public void setDelete(Boolean delete) {
        this.delete = delete;
    }

    public IamProjectStatusVO getIamProjectStatusVO() {
        return iamProjectStatusVO;
    }

    public void setIamProjectStatusVO(IamProjectStatusVO iamProjectStatusVO) {
        this.iamProjectStatusVO = iamProjectStatusVO;
    }
}
