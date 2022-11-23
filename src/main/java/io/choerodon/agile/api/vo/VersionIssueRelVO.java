package io.choerodon.agile.api.vo;


import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.annotations.ApiModelProperty;

import io.choerodon.agile.infra.utils.StringUtil;

import org.hzero.starter.keyencrypt.core.Encrypt;
/**
 * @author dinghuang123@gmail.com
 * @since 2018-05-15 16:21:18
 */
@JsonInclude(JsonInclude.Include.NON_NULL)
public class VersionIssueRelVO implements Serializable {

    @ApiModelProperty(value = "版本id")
    @Encrypt
    private Long versionId;

    @ApiModelProperty(value = "问题id")
    @Encrypt
    private Long issueId;

    @ApiModelProperty(value = "版本名称")
    private String name;

    @ApiModelProperty(value = "项目id")
    private Long projectId;

    @ApiModelProperty(value = "版本关系：fix、influence")
    private String relationType;

    @ApiModelProperty(value = "版本状态")
    private String statusCode;

    public Long getVersionId() {
        return versionId;
    }

    public VersionIssueRelVO setVersionId(Long versionId) {
        this.versionId = versionId;
        return this;
    }

    public Long getIssueId() {
        return issueId;
    }

    public VersionIssueRelVO setIssueId(Long issueId) {
        this.issueId = issueId;
        return this;
    }

    public String getName() {
        return name;
    }

    public VersionIssueRelVO setName(String name) {
        this.name = name;
        return this;
    }

    public Long getProjectId() {
        return projectId;
    }

    public VersionIssueRelVO setProjectId(Long projectId) {
        this.projectId = projectId;
        return this;
    }

    public String getRelationType() {
        return relationType;
    }

    public VersionIssueRelVO setRelationType(String relationType) {
        this.relationType = relationType;
        return this;
    }

    public String getStatusCode() {
        return statusCode;
    }

    public VersionIssueRelVO setStatusCode(String statusCode) {
        this.statusCode = statusCode;
        return this;
    }

    @Override
    public String toString() {
        return StringUtil.getToString(this);
    }

}
