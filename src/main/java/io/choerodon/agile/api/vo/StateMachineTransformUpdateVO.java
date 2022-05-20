package io.choerodon.agile.api.vo;

import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

/**
 * @author zhaotianxin
 * @date 2020-08-11 13:37
 */
public class StateMachineTransformUpdateVO {
    @Encrypt
    @ApiModelProperty(value = "开始节点id")
    private Long startNodeId;
    @Encrypt
    @ApiModelProperty(value = "结束节点id")
    private Long endNodeId;
    @ApiModelProperty(value = "开始状态名")
    private String startStatusName;
    @ApiModelProperty(value = "结束状态名")
    private String endStatusName;
    @ApiModelProperty(value = "是否被选择")
    private Boolean select;

    public Long getStartNodeId() {
        return startNodeId;
    }

    public void setStartNodeId(Long startNodeId) {
        this.startNodeId = startNodeId;
    }

    public Long getEndNodeId() {
        return endNodeId;
    }

    public void setEndNodeId(Long endNodeId) {
        this.endNodeId = endNodeId;
    }

    public Boolean getSelect() {
        return select;
    }

    public void setSelect(Boolean select) {
        this.select = select;
    }

    public String getStartStatusName() {
        return startStatusName;
    }

    public void setStartStatusName(String startStatusName) {
        this.startStatusName = startStatusName;
    }

    public String getEndStatusName() {
        return endStatusName;
    }

    public void setEndStatusName(String endStatusName) {
        this.endStatusName = endStatusName;
    }
}
