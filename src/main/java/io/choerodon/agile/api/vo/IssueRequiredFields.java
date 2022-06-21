package io.choerodon.agile.api.vo;

import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

import java.util.List;

/**
 * @author huaxin.deng@hand-china.com
 * @since 2022/6/20
 */
public class IssueRequiredFields {

    @Encrypt
    @ApiModelProperty(value = "issue id")
    private Long issueId;

    @ApiModelProperty(value = "必填字段")
    private List<PageFieldViewVO> requiredFields;

    public Long getIssueId() {
        return issueId;
    }

    public void setIssueId(Long issueId) {
        this.issueId = issueId;
    }

    public List<PageFieldViewVO> getRequiredFields() {
        return requiredFields;
    }

    public void setRequiredFields(List<PageFieldViewVO> requiredFields) {
        this.requiredFields = requiredFields;
    }
}
