package io.choerodon.agile.api.vo;

import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

/**
 * @author zhaotianxin
 * @date 2021-05-11 11:33
 */
public class TestStatusVO {

    @ApiModelProperty(value = "主键状态ID")
    @Encrypt
    private Long statusId;

    @ApiModelProperty(value = "状态名")
    private String statusName;

    @ApiModelProperty(value = "状态颜色rgba")
    private String statusColor;

    public Long getStatusId() {
        return statusId;
    }

    public void setStatusId(Long statusId) {
        this.statusId = statusId;
    }

    public String getStatusName() {
        return statusName;
    }

    public void setStatusName(String statusName) {
        this.statusName = statusName;
    }

    public String getStatusColor() {
        return statusColor;
    }

    public void setStatusColor(String statusColor) {
        this.statusColor = statusColor;
    }
}
