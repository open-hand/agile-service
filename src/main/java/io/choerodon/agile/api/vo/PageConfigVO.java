package io.choerodon.agile.api.vo;

import java.util.List;

/**
 * @author superlee
 * @since 2020-08-10
 */
public class PageConfigVO {

    private List<PageConfigFieldVO> fields;

    private IssueTypeFieldVO issueTypeFieldVO;

    public List<PageConfigFieldVO> getFields() {
        return fields;
    }

    public void setFields(List<PageConfigFieldVO> fields) {
        this.fields = fields;
    }

    public IssueTypeFieldVO getIssueTypeFieldVO() {
        return issueTypeFieldVO;
    }

    public void setIssueTypeFieldVO(IssueTypeFieldVO issueTypeFieldVO) {
        this.issueTypeFieldVO = issueTypeFieldVO;
    }
}
