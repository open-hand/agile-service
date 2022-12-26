package io.choerodon.agile.api.vo.business;


import javax.validation.constraints.NotBlank;
import javax.validation.constraints.Pattern;

import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import org.hibernate.validator.constraints.Length;

import io.choerodon.mybatis.domain.AuditDomain;

import org.hzero.starter.keyencrypt.core.Encrypt;

@ApiModel(value = "工时日志标签VO")
@JsonInclude(JsonInclude.Include.NON_NULL)
public class WorkHoursLabelVO extends AuditDomain {

    @Encrypt
    @ApiModelProperty(value = "id")
    private Long id;

    @ApiModelProperty(value = "组织Id")
    private Long organizationId;

    @Length(max = 100)
    @Pattern(regexp = "^[a-zA-Z0-9]([_a-zA-Z0-9]*[a-zA-Z0-9])?$")
    @ApiModelProperty(value = "标签编码")
    private String code;

    @NotBlank
    @Length(max = 255)
    @ApiModelProperty(value = "标签名称")
    private String name;

    @ApiModelProperty(value = "是否启用，true表示启用，false表示未启用")
    private Boolean enabledFlag;

    @ApiModelProperty(value = "模糊查询参数")
    private String param;

    public Long getId() {
        return id;
    }

    public WorkHoursLabelVO setId(Long id) {
        this.id = id;
        return this;
    }

    public Long getOrganizationId() {
        return organizationId;
    }

    public WorkHoursLabelVO setOrganizationId(Long organizationId) {
        this.organizationId = organizationId;
        return this;
    }

    public String getCode() {
        return code;
    }

    public WorkHoursLabelVO setCode(String code) {
        this.code = code;
        return this;
    }

    public String getName() {
        return name;
    }

    public WorkHoursLabelVO setName(String name) {
        this.name = name;
        return this;
    }

    public Boolean getEnabledFlag() {
        return enabledFlag;
    }

    public WorkHoursLabelVO setEnabledFlag(Boolean enabledFlag) {
        this.enabledFlag = enabledFlag;
        return this;
    }

    public String getParam() {
        return param;
    }

    public WorkHoursLabelVO setParam(String param) {
        this.param = param;
        return this;
    }
}
