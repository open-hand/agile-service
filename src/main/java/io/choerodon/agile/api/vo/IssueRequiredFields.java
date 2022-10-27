package io.choerodon.agile.api.vo;

import java.util.List;

import io.swagger.annotations.ApiModelProperty;

import org.hzero.starter.keyencrypt.core.Encrypt;

/**
 * @author huaxin.deng@hand-china.com
 * @since 2022/6/20
 */
public class IssueRequiredFields {

    @Encrypt
    @ApiModelProperty(value = "issue id")
    private Long issueId;

    @ApiModelProperty(value = "问题编号")
    private String issueNum;

    @ApiModelProperty(value = "问题概要")
    private String summary;

    @ApiModelProperty(value = "必填字段")
    private List<PageFieldViewVO> requiredFields;
    @ApiModelProperty(value = "问题类型")
    private IssueTypeVO issueTypeVO;

    /**
     * @return issue id
     */
    public Long getIssueId() {
        return issueId;
    }

    public IssueRequiredFields setIssueId(Long issueId) {
        this.issueId = issueId;
        return this;
    }

    /**
     * @return 问题编号
     */
    public String getIssueNum() {
        return this.issueNum;
    }

    public IssueRequiredFields setIssueNum(String issueNum) {
        this.issueNum = issueNum;
        return this;
    }

    /**
     * @return 问题概要
     */
    public String getSummary() {
        return this.summary;
    }

    public IssueRequiredFields setSummary(String summary) {
        this.summary = summary;
        return this;
    }

    /**
     * @return 必填字段
     */
    public List<PageFieldViewVO> getRequiredFields() {
        return requiredFields;
    }

    public IssueRequiredFields setRequiredFields(List<PageFieldViewVO> requiredFields) {
        this.requiredFields = requiredFields;
        return this;
    }

    public IssueTypeVO getIssueTypeVO() {
        return issueTypeVO;
    }

    public void setIssueTypeVO(IssueTypeVO issueTypeVO) {
        this.issueTypeVO = issueTypeVO;
    }
}
