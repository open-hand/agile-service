package io.choerodon.agile.api.vo;

import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

import java.util.Set;

/**
 * @author huaxin.deng@hand-china.com 2021-02-22 11:27:55
 */
public class IssueCountWithStatusIdsVO {

    @Encrypt
    @ApiModelProperty(value = "状态id集合")
    private Set<Long> statusIds;
    @ApiModelProperty(value = "问题数量")
    private Integer count;

    @Encrypt
    @ApiModelProperty(value = "问题id集合")
    private Set<Long> issueTypeIds;

    public IssueCountWithStatusIdsVO(Set<Long> statusIds, Integer count, Set<Long> issueTypeIds) {
        this.statusIds = statusIds;
        this.count = count;
        this.issueTypeIds = issueTypeIds;
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

    public Set<Long> getIssueTypeIds() {
        return issueTypeIds;
    }

    public void setIssueTypeIds(Set<Long> issueTypeIds) {
        this.issueTypeIds = issueTypeIds;
    }
}
