package io.choerodon.agile.api.vo;

import io.swagger.annotations.ApiModelProperty;

import java.util.List;

/**
 * @author superlee
 * @since 2021-07-01
 */
public class IssueLinkResponseVO {
    @ApiModelProperty(value = "状态联动")
    private List<IssueLinkVO> issueLinks;
    @ApiModelProperty(value = "影响的问题id")
    private List<Long> influenceIssueIds;

    public List<IssueLinkVO> getIssueLinks() {
        return issueLinks;
    }

    public void setIssueLinks(List<IssueLinkVO> issueLinks) {
        this.issueLinks = issueLinks;
    }

    public List<Long> getInfluenceIssueIds() {
        return influenceIssueIds;
    }

    public void setInfluenceIssueIds(List<Long> influenceIssueIds) {
        this.influenceIssueIds = influenceIssueIds;
    }
}
