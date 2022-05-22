package io.choerodon.agile.api.vo;

import io.swagger.annotations.ApiModelProperty;

/**
 * @author chihao.ran@hand-china.com
 * 2021/08/12 19:27
 */
public class IssueCountStatusVO {
    @ApiModelProperty(value = "状态id")
    private Long statusId;
    @ApiModelProperty(value = "问题数量")
    private Long issueCount;

    public Long getStatusId() {
        return statusId;
    }

    public void setStatusId(Long statusId) {
        this.statusId = statusId;
    }

    public Long getIssueCount() {
        return issueCount;
    }

    public void setIssueCount(Long issueCount) {
        this.issueCount = issueCount;
    }
}
