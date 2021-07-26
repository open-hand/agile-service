package io.choerodon.agile.api.vo;

/**
 * @author zhaotianxin
 * @date 2021-07-26 13:44
 */
public class SprintStartMessageVO {
    private Integer noStoryPointIssue;

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
