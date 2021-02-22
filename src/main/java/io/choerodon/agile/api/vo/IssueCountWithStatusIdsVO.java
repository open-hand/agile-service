package io.choerodon.agile.api.vo;

import org.hzero.starter.keyencrypt.core.Encrypt;

import java.util.Set;

/**
 * @author huaxin.deng@hand-china.com 2021-02-22 11:27:55
 */
public class IssueCountWithStatusIdsVO {

    @Encrypt
    private Set<Long> statusIds;

    private Integer count;

    public IssueCountWithStatusIdsVO(Set<Long> statusIds, Integer count) {
        this.statusIds = statusIds;
        this.count = count;
    }

    public Set<Long> getStatusIds() {
        return statusIds;
    }

    public void setStatusIds(Set<Long> statusIds) {
        this.statusIds = statusIds;
    }

    public Integer getCount() {
        return count;
    }

    public void setCount(Integer count) {
        this.count = count;
    }
}
