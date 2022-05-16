package io.choerodon.agile.api.vo;

import io.swagger.annotations.ApiModelProperty;

import java.util.List;

/**
 * @author chihao.ran@hand-china.com
 * 2021/07/20 16:35
 */
public class PageTemplateVO {
    @ApiModelProperty(value = "页面模版字段")
    private List<PageTemplateFieldVO> fields;
    @ApiModelProperty(value = "页面模版问题类型字段")
    private IssueTypeFieldVO issueTypeFieldVO;

    public List<PageTemplateFieldVO> getFields() {
        return fields;
    }

    public void setFields(List<PageTemplateFieldVO> fields) {
        this.fields = fields;
    }

    public IssueTypeFieldVO getIssueTypeFieldVO() {
        return issueTypeFieldVO;
    }

    public void setIssueTypeFieldVO(IssueTypeFieldVO issueTypeFieldVO) {
        this.issueTypeFieldVO = issueTypeFieldVO;
    }
}
