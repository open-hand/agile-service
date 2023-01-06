package io.choerodon.agile.api.vo.business;


import javax.validation.constraints.NotBlank;
import javax.validation.constraints.Pattern;

import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import org.hibernate.validator.constraints.Length;

import io.choerodon.mybatis.domain.AuditDomain;

import org.hzero.starter.keyencrypt.core.Encrypt;

/**
 * 工时日志类型VO
 * @author gaokuo.dai@zknow.com 2022-12-22
 */
@ApiModel(value = "工时日志类型VO")
@JsonInclude(JsonInclude.Include.NON_NULL)
public class WorkHoursTypeVO extends AuditDomain {

    @Encrypt
    @ApiModelProperty(value = "id")
    private Long id;

    @ApiModelProperty(value = "组织Id")
    private Long organizationId;

    @Length(max = 100)
    @Pattern(regexp = "^[a-zA-Z0-9]([_a-zA-Z0-9]*[a-zA-Z0-9])?$")
    @ApiModelProperty(value = "类型编码")
    private String code;

    @NotBlank
    @ApiModelProperty(value = "类型名称")
    private String name;

    @NotBlank
    @ApiModelProperty(value = "类型颜色")
    private String color;

    @ApiModelProperty(value = "是否启用，true表示启用，false表示未启用")
    private Boolean enabledFlag;

    @ApiModelProperty(value = "模糊查询参数")
    private String param;

    public Long getId() {
        return id;
    }

    public WorkHoursTypeVO setId(Long id) {
        this.id = id;
        return this;
    }

    public Long getOrganizationId() {
        return organizationId;
    }

    public WorkHoursTypeVO setOrganizationId(Long organizationId) {
        this.organizationId = organizationId;
        return this;
    }

    public String getCode() {
        return code;
    }

    public WorkHoursTypeVO setCode(String code) {
        this.code = code;
        return this;
    }

    public String getName() {
        return name;
    }

    public WorkHoursTypeVO setName(String name) {
        this.name = name;
        return this;
    }

    public String getColor() {
        return color;
    }

    public WorkHoursTypeVO setColor(String color) {
        this.color = color;
        return this;
    }

    public Boolean getEnabledFlag() {
        return enabledFlag;
    }

    public WorkHoursTypeVO setEnabledFlag(Boolean enabledFlag) {
        this.enabledFlag = enabledFlag;
        return this;
    }

    public String getParam() {
        return param;
    }

    public WorkHoursTypeVO setParam(String param) {
        this.param = param;
        return this;
    }
}
