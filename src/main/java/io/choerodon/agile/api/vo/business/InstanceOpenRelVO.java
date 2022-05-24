package io.choerodon.agile.api.vo.business;

import io.swagger.annotations.ApiModelProperty;

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
