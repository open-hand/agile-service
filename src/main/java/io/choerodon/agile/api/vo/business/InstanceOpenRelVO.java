package io.choerodon.agile.api.vo.business;

import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

/**
 * @author huaxin.deng@hand-china.com
 * @since 2022/5/24
 */
public class InstanceOpenRelVO {

    @ApiModelProperty(value = "第三方实例id")
    private Long openInstanceId;

    @ApiModelProperty(value = "第三方实例编号")
    private String openInstanceNum;

    @ApiModelProperty(value = "来源：如yqcloud等")
    private String source;
    @Encrypt
    @ApiModelProperty(value = "实例id")
    private Long instanceId;
    @ApiModelProperty(value = "实例类型")
    private String instanceType;

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
}
