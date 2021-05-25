package io.choerodon.agile.api.vo;

import org.hzero.starter.keyencrypt.core.Encrypt;

/**
 * @author zhaotianxin
 * @date 2020-11-25 9:58
 */
public class IssueProgressVO {
    @Encrypt
    private Long id;

    private Integer unCompletedCount;

    private Integer completedCount;

    private Integer totalCount;

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public Integer getUnCompletedCount() {
        return unCompletedCount;
    }

    public void setUnCompletedCount(Integer unCompletedCount) {
        this.unCompletedCount = unCompletedCount;
    }

    public Integer getCompletedCount() {
        return completedCount;
    }

    public void setCompletedCount(Integer completedCount) {
        this.completedCount = completedCount;
    }

    public Integer getTotalCount() {
        return totalCount;
    }

    public void setTotalCount(Integer totalCount) {
        this.totalCount = totalCount;
    }
}
