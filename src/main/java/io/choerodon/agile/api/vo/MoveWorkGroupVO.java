package io.choerodon.agile.api.vo;

import org.hzero.starter.keyencrypt.core.Encrypt;

/**
 * @author zhaotianxin
 * @date 2021-11-08 17:15
 */
public class MoveWorkGroupVO {
    @Encrypt(ignoreValue = {"0"})
    private Long parentId;

    @Encrypt
    private Long workGroupId;

    private Boolean before;

    @Encrypt(ignoreValue = {"0"})
    private Long  outSetId;

    public Long getParentId() {
        return parentId;
    }

    public void setParentId(Long parentId) {
        this.parentId = parentId;
    }

    public Long getWorkGroupId() {
        return workGroupId;
    }

    public void setWorkGroupId(Long workGroupId) {
        this.workGroupId = workGroupId;
    }

    public Boolean getBefore() {
        return before;
    }

    public void setBefore(Boolean before) {
        this.before = before;
    }

    public Long getOutSetId() {
        return outSetId;
    }

    public void setOutSetId(Long outSetId) {
        this.outSetId = outSetId;
    }
}
