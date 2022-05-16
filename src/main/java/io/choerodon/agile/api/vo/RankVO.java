package io.choerodon.agile.api.vo;


import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

/**
 * Created by HuangFuqiang@choerodon.io on 2019/6/24.
 * Email: fuqianghuang01@gmail.com
 */
public class RankVO {
    @ApiModelProperty(value = "项目id")
    private Long projectId;
    @ApiModelProperty(value = "类型")
    private String type;
    @ApiModelProperty(value = "是否在之前")
    private Boolean before;

    @Encrypt
    @ApiModelProperty(value = "引用的问题id")
    private Long referenceIssueId;

    @Encrypt
    @ApiModelProperty(value = "问题id")
    private Long issueId;
    @ApiModelProperty(value = "乐观锁")
    private Long objectVersionNumber;

    public Long getProjectId() {
        return projectId;
    }

    public void setProjectId(Long projectId) {
        this.projectId = projectId;
    }

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    public Boolean getBefore() {
        return before;
    }

    public void setBefore(Boolean before) {
        this.before = before;
    }

    public Long getReferenceIssueId() {
        return referenceIssueId;
    }

    public void setReferenceIssueId(Long referenceIssueId) {
        this.referenceIssueId = referenceIssueId;
    }

    public void setIssueId(Long issueId) {
        this.issueId = issueId;
    }

    public Long getIssueId() {
        return issueId;
    }

    public void setObjectVersionNumber(Long objectVersionNumber) {
        this.objectVersionNumber = objectVersionNumber;
    }

    public Long getObjectVersionNumber() {
        return objectVersionNumber;
    }
}
