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

    public void setInstanceId(Long instanceId) {
        this.instanceId = instanceId;
    }

    public String getInstanceType() {
        return instanceType;
    }

    public void setInstanceType(String instanceType) {
        this.instanceType = instanceType;
    }

    public Long getOpenInstanceId() {
        return openInstanceId;
    }

    public void setOpenInstanceId(Long openInstanceId) {
        this.openInstanceId = openInstanceId;
    }

    public String getOpenInstanceNum() {
        return openInstanceNum;
    }

    public void setOpenInstanceNum(String openInstanceNum) {
        this.openInstanceNum = openInstanceNum;
    }

    public String getSource() {
        return source;
    }

    public void setSource(String source) {
        this.source = source;
    }

    public Long getProjectId() {
        return projectId;
    }

    public void setProjectId(Long projectId) {
        this.projectId = projectId;
    }

    public Object getThirdPartyProperties() {
        return thirdPartyProperties;
    }

    public void setThirdPartyProperties(Object thirdPartyProperties) {
        this.thirdPartyProperties = thirdPartyProperties;
    }
}
