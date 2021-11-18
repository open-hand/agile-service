package io.choerodon.agile.api.vo;

import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

import java.util.List;

/**
 * @author zhaotianxin
 * @date 2021-11-18 11:25
 */
public class StatusParamVO {

    @Encrypt
    @ApiModelProperty("忽略查询的状态")
    private List<Long> ignoredStatusIds;

    @ApiModelProperty("是否将忽略的状态放在最前面")
    private Boolean queryIgnored;

    @ApiModelProperty("状态查询")
    private String param;

    public List<Long> getIgnoredStatusIds() {
        return ignoredStatusIds;
    }

    public void setIgnoredStatusIds(List<Long> ignoredStatusIds) {
        this.ignoredStatusIds = ignoredStatusIds;
    }

    public String getParam() {
        return param;
    }

    public void setParam(String param) {
        this.param = param;
    }

    public Boolean getQueryIgnored() {
        return queryIgnored;
    }

    public void setQueryIgnored(Boolean queryIgnored) {
        this.queryIgnored = queryIgnored;
    }
}
