package io.choerodon.agile.api.vo;


/**
 * @author superlee
 * @since 2020-10-22
 */
public class WorkBenchIssueSearchVO {

    private String type;

    private SearchVO searchVO;

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    public SearchVO getSearchVO() {
        return searchVO;
    }

    public void setSearchVO(SearchVO searchVO) {
        this.searchVO = searchVO;
    }
}
