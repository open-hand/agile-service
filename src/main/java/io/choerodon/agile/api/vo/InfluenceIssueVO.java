package io.choerodon.agile.api.vo;

import java.util.List;

/**
 * @author zhaotianxin
 * @date 2021-07-13 15:34
 */
public class InfluenceIssueVO {
    private Long issueId;

    private Long statusId;

    private Boolean loop;

    private Long linkSettingId;

    private Boolean childrenTriggered;

    private List<InfluenceIssueVO> childrenVO;

    public Long getIssueId() {
        return issueId;
    }

    public void setIssueId(Long issueId) {
        this.issueId = issueId;
    }

    public Long getStatusId() {
        return statusId;
    }

    public void setStatusId(Long statusId) {
        this.statusId = statusId;
    }

    public List<InfluenceIssueVO> getChildrenVO() {
        return childrenVO;
    }

    public void setChildrenVO(List<InfluenceIssueVO> childrenVO) {
        this.childrenVO = childrenVO;
    }

    public Boolean getLoop() {
        return loop;
    }

    public void setLoop(Boolean loop) {
        this.loop = loop;
    }

    public Long getLinkSettingId() {
        return linkSettingId;
    }

    public void setLinkSettingId(Long linkSettingId) {
        this.linkSettingId = linkSettingId;
    }

    public Boolean getChildrenTriggered() {
        return childrenTriggered;
    }

    public void setChildrenTriggered(Boolean childrenTriggered) {
        this.childrenTriggered = childrenTriggered;
    }
}
