package io.choerodon.agile.api.vo;

import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

/**
 * @author superlee
 * @since 2021-10-21
 */
public class EstimatedTimeConflictVO {
    @Encrypt
    @ApiModelProperty("用户id")
    private Long userId;
    @ApiModelProperty("是否冲突")
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
