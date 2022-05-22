package io.choerodon.agile.api.vo;

import io.swagger.annotations.ApiModelProperty;

import java.util.List;

/**
 * @author zhaotianxin
 * @date 2021-07-13 15:34
 */
public class InfluenceIssueVO {
    @ApiModelProperty(value = "问题id")
    private Long issueId;
    @ApiModelProperty(value = "状态id")
    private Long statusId;
    @ApiModelProperty(value = "是否成环")
    private Boolean loop;
    @ApiModelProperty(value = "关联的配置id")
    private Long linkageSettingId;
    @ApiModelProperty(value = "层级")
    private Integer level;
    @ApiModelProperty(value = "最大的链路深度")
    private Boolean maxDepth;
    @ApiModelProperty(value = "是否触发子级")
    private Boolean childrenTriggered;
    @ApiModelProperty(value = "筛选条件")
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

    public Long getLinkageSettingId() {
        return linkageSettingId;
    }

    public void setLinkageSettingId(Long linkageSettingId) {
        this.linkageSettingId = linkageSettingId;
    }

    public Boolean getChildrenTriggered() {
        return childrenTriggered;
    }

    public void setChildrenTriggered(Boolean childrenTriggered) {
        this.childrenTriggered = childrenTriggered;
    }


    public Integer getLevel() {
        return level;
    }

    public void setLevel(Integer level) {
        this.level = level;
    }

    public Boolean getMaxDepth() {
        return maxDepth;
    }

    public void setMaxDepth(Boolean maxDepth) {
        this.maxDepth = maxDepth;
    }
}
