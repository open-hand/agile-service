package io.choerodon.agile.api.vo;

import org.hzero.starter.keyencrypt.core.Encrypt;

/**
 * @author superlee
 * @since 2021-10-21
 */
public class EstimatedTimeConflictVO {
    @Encrypt
    private Long userId;

    private Boolean conflicted;

    public Long getUserId() {
        return userId;
    }

    public void setUserId(Long userId) {
        this.userId = userId;
    }

    public Boolean getConflicted() {
        return conflicted;
    }

    public void setConflicted(Boolean conflicted) {
        this.conflicted = conflicted;
    }
}
