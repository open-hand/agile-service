package io.choerodon.agile.api.vo;

import io.swagger.annotations.ApiModelProperty;

/**
 * @author zhaotianxin
 * @date 2021-07-26 13:44
 */
public class SprintStartMessageVO {
    @ApiModelProperty(value = "没有故事点的问题")
    private Integer noStoryPointIssue;
    @ApiModelProperty(value = "没有预计时间的问题")
    private Integer noRemainingTimeIssue;

    public Integer getNoStoryPointIssue() {
        return noStoryPointIssue;
    }

    public void setNoStoryPointIssue(Integer noStoryPointIssue) {
        this.noStoryPointIssue = noStoryPointIssue;
    }

    public Integer getNoRemainingTimeIssue() {
        return noRemainingTimeIssue;
    }

    public void setNoRemainingTimeIssue(Integer noRemainingTimeIssue) {
        this.noRemainingTimeIssue = noRemainingTimeIssue;
    }
}
