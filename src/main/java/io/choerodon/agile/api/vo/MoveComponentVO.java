package io.choerodon.agile.api.vo;

import org.hzero.starter.keyencrypt.core.Encrypt;

import java.util.List;

/**
 * @author zhaotianxin
 * @date 2021-01-18 13:41
 */
public class MoveComponentVO {

    private Boolean before;

    @Encrypt
    private List<Long> componentIds;

    @Encrypt
    private Long outsetId;

    public Boolean getBefore() {
        return before;
    }

    public void setBefore(Boolean before) {
        this.before = before;
    }

    public List<Long> getComponentIds() {
        return componentIds;
    }

    public void setComponentIds(List<Long> componentIds) {
        this.componentIds = componentIds;
    }

    public Long getOutsetId() {
        return outsetId;
    }

    public void setOutsetId(Long outsetId) {
        this.outsetId = outsetId;
    }
}
