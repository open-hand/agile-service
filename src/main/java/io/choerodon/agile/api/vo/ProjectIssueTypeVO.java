package io.choerodon.agile.api.vo;

import io.swagger.annotations.ApiModelProperty;

/**
 * @author superlee
 * @since 2021-01-20
 */
public class ProjectIssueTypeVO extends ProjectVO {
    @ApiModelProperty(value = "问题类型是否启用")
    private Boolean issueTypeEnabled;

    public Boolean getIssueTypeEnabled() {
        return issueTypeEnabled;
    }

    public void setIssueTypeEnabled(Boolean issueTypeEnabled) {
        this.issueTypeEnabled = issueTypeEnabled;
    }
}
