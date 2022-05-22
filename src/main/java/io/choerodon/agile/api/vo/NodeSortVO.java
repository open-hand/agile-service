package io.choerodon.agile.api.vo;

import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

/**
 * @author zhaotianxin
 * @date 2021-03-02 14:09
 */
public class NodeSortVO {
    @ApiModelProperty(value = "是否在前面")
    private Boolean before;

    @Encrypt
    @ApiModelProperty(value = "outSetId")
    private Long outSetId;

    @Encrypt
    @ApiModelProperty(value = "问题类型id")
    private Long issueTypeId;

    @Encrypt
    @ApiModelProperty(value = "节点id")
    private Long nodeId;

    public Boolean getBefore() {
        return before;
    }

    public void setBefore(Boolean before) {
        this.before = before;
    }

    public Long getOutSetId() {
        return outSetId;
    }

    public void setOutSetId(Long outSetId) {
        this.outSetId = outSetId;
    }

    public Long getIssueTypeId() {
        return issueTypeId;
    }

    public void setIssueTypeId(Long issueTypeId) {
        this.issueTypeId = issueTypeId;
    }

    public Long getNodeId() {
        return nodeId;
    }

    public void setNodeId(Long nodeId) {
        this.nodeId = nodeId;
    }
}
