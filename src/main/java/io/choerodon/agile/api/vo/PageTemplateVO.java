package io.choerodon.agile.api.vo;

import java.util.List;

/**
 * @author chihao.ran@hand-china.com
 * 2021/07/20 16:35
 */
public class PageTemplateVO {
    private List<PageTemplateFieldVO> fields;

    private IssueTypeFieldVO issueTypeFieldVO;

    public java.util.List<PageTemplateFieldVO> getFields() {
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
