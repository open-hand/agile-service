package io.choerodon.agile.api.vo.business;

import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

/**
 * @author huaxin.deng@hand-china.com
 * @since 2022/4/12
 */
public class RiskIssueRelVO {

    @ApiModelProperty(value = "主键id")
    @Encrypt
    private Long id;

    @ApiModelProperty(value = "问题id")
    @Encrypt
    private Long issueId;

    @ApiModelProperty(value = "关联问题id")
    @Encrypt
    private Long relatedIssueId;

    @ApiModelProperty(value = "关联类型")
    private String type;

    @ApiModelProperty(value = "项目id")
    private Long projectId;

    @ApiModelProperty(value = "组织id")
    private Long organizationId;

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public Long getIssueId() {
        return issueId;
    }

    public void setIssueId(Long issueId) {
        this.issueId = issueId;
    }

    public Long getRelatedIssueId() {
        return relatedIssueId;
    }

    public void setRelatedIssueId(Long relatedIssueId) {
        this.relatedIssueId = relatedIssueId;
    }

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    public Long getProjectId() {
        return projectId;
    }

    public void setProjectId(Long projectId) {
        this.projectId = projectId;
    }

    public Long getOrganizationId() {
        return organizationId;
    }

    public void setOrganizationId(Long organizationId) {
        this.organizationId = organizationId;
    }

    @Override
    public String toString() {
        return "RiskIssueRelVO{" +
                "id=" + id +
                ", issueId=" + issueId +
                ", relatedIssueId=" + relatedIssueId +
                ", type='" + type + '\'' +
                ", projectId=" + projectId +
                ", organizationId=" + organizationId +
                '}';
    }
}
