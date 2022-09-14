package io.choerodon.agile.api.vo.business;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;

import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.annotations.ApiModelProperty;

import org.hzero.starter.keyencrypt.core.Encrypt;

/**
 * @author huaxin.deng@hand-china.com
 * @since 2022/5/24
 */
@JsonInclude(JsonInclude.Include.NON_NULL)
public class InstanceOpenRelVO {

    @ApiModelProperty("项目id")
    @NotNull
    private Long projectId;

    @ApiModelProperty("组织id")
    @NotNull
    private Long organizationId;

    @Encrypt
    @NotNull
    @ApiModelProperty(value = "实例id")
    private Long instanceId;

    @ApiModelProperty(value = "实例类型")
    @NotBlank
    private String instanceType;

    @ApiModelProperty(value = "第三方实例id")
    private Long openInstanceId;

    @ApiModelProperty(value = "第三方实例编号")
    private String openInstanceNum;

    @ApiModelProperty(value = "来源：如yqcloud等")
    @NotBlank
    private String source;

    @ApiModelProperty(value = "第三方属性, 暂时直接使用obj，为后续拓展准备")
    private Object thirdPartyProperties;

    public Long getInstanceId() {
        return instanceId;
    }

    public InstanceOpenRelVO setInstanceId(Long instanceId) {
        this.instanceId = instanceId;
        return this;
    }

    public String getInstanceType() {
        return instanceType;
    }

    public InstanceOpenRelVO setInstanceType(String instanceType) {
        this.instanceType = instanceType;
        return this;
    }

    public Long getOpenInstanceId() {
        return openInstanceId;
    }

    public InstanceOpenRelVO setOpenInstanceId(Long openInstanceId) {
        this.openInstanceId = openInstanceId;
        return this;
    }

    public String getOpenInstanceNum() {
        return openInstanceNum;
    }

    public InstanceOpenRelVO setOpenInstanceNum(String openInstanceNum) {
        this.openInstanceNum = openInstanceNum;
        return this;
    }

    public String getSource() {
        return source;
    }

    public InstanceOpenRelVO setSource(String source) {
        this.source = source;
        return this;
    }

    public Long getProjectId() {
        return projectId;
    }

    public InstanceOpenRelVO setProjectId(Long projectId) {
        this.projectId = projectId;
        return this;
    }

    /**
     * @return 组织ID
     */
    public Long getOrganizationId() {
        return organizationId;
    }

    public InstanceOpenRelVO setOrganizationId(Long organizationId) {
        this.organizationId = organizationId;
        return this;
    }

    public Object getThirdPartyProperties() {
        return thirdPartyProperties;
    }

    public InstanceOpenRelVO setThirdPartyProperties(Object thirdPartyProperties) {
        this.thirdPartyProperties = thirdPartyProperties;
        return this;
    }
}
