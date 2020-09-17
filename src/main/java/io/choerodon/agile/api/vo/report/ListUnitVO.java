package io.choerodon.agile.api.vo.report;

import java.util.List;

import io.choerodon.agile.api.vo.SearchVO;

/**
 * @author jiaxu.cui@hand-china.com 2020/9/15 下午7:02
 */
public class ListUnitVO extends ReportUnitVO {

    private List<String> colList;

    private List<Long> issueIdList;

    private SearchVO searchVO;

    public List<String> getColList() {
        return colList;
    }

    public void setColList(List<String> colList) {
        this.colList = colList;
    }

    public List<Long> getIssueIdList() {
        return issueIdList;
    }

    public void setIssueIdList(List<Long> issueIdList) {
        this.issueIdList = issueIdList;
    }

    public SearchVO getSearchVO() {
        return searchVO;
    }

    public void setSearchVO(SearchVO searchVO) {
        this.searchVO = searchVO;
    }
}
